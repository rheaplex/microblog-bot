;; microblog-bot.lisp -  Bots for microblogs (Twitter, Laconica).
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; FIXME - If the user hasn't posted yet, most recent id will be null
;;         disambiguate this and allow the first post to be caught.
;;         Just set it back to zero if it comes back null?
;;         Or might that result in problems if the network is unavailable? Yes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:microblog-bot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-microblog-service (server-url from-source)
  "Set the server and the 'from..' string displayed on messages to that server."
  (setf twit::*source* from-source)
  (setf twit::*base-url* server-url))

(defclass microblog-user ()
  ((user-nickname :reader user-nickname
		  :initarg :nickname)
   (user-password :reader user-password
		  :initarg :password)
   (most-recent-mention-id :accessor most-recent-mention-id 
			   :initarg :most-recent-mention-id 
			   :initform 0)))

(defvar *live* t)

(defmacro with-microblog-user (user &body body)
  "Log in, execture the body, log out."
  `(progn
     (cl-twit:with-session ((user-nickname ,user) 
			    (user-password ,user) 
			    :authenticatep t)
       ,@body)
     ;; with-session doesn't currently do this, remove if it ever does.
     (cl-twit:logout)))

(defmacro update-id (place item)
  "Change the id at place for item's id if higher"
  (let ((new-id (gensym)))
    `(let ((,new-id (cl-twit::id ,item))) 
       (when (> ,new-id ,place)
	 (setf ,place ,new-id)))))

(defun post (message)
  (cl-twit:update message))

(defun set-debug ()
  (setf *live* nil)
  (setf (symbol-function 'post) #'(lambda(message)
				    (format t "~a~%" message))))

(defun set-live ()
  (setf *live* t)
  (setf (symbol-function 'post) #'(lambda (message)
	       (cl-twit:update message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic bot
;; TODO Split out into constant and intermittent bots, or tasks for a bot?
;; Tasks are probably best
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %one-hour (* 60 60)
  "One hour in seconds")

(defconstant %one-day (* %one-hour 24)
  "One day in seconds")

(defvar *bot-sleep-time* (* 60 5)
  "How long to wait between each check")

(defclass microblog-bot (microblog-user)
  ((wait-remaining :accessor wait-remaining 
		   :initarg :wait-remaining
		   :initform 0)
   (previous-day :accessor previous-day 
		 :initarg :previous-day
		 :initform 0)
   (min-wait :accessor min-wait 
	     :initarg :min-wait
	     :initform (* 60 60 1))
   (max-wait :accessor max-wait 
	     :initarg :max-wait 
	     :initform (* 60 60 4))
   (daily-task-hour :accessor daily-task-hour
		    :initarg :daily-task-hour 
		    :initform 0)   
   (source-url :accessor source-url
		    :initarg :source-url
		    :allocation :class
		    :initform nil)
   (ignore-messages-from :accessor ignore-messages-from 
	     :initarg :ignore
	     :initform '())))

(defmethod reset-wait ((bot microblog-bot))
  "Set the bot's next wait period to between min-wait..max-wait, and wait to 0."
  (setf (wait-remaining bot)
	(+ (min-wait bot)
	   (random (- (max-wait bot)
		      (min-wait bot))))))

(defun current-user-replies-after-id ()
  "Get the exclusive lower bound for replies to the user to check."
  ;; Wasteful, as we only want the most recent id
  ;; If the user hasn't posted yet, start with the most recent global id
  (or (cl-twit::get-newest-id (cl-twit:m-replies))
      (cl-twit::get-newest-id (cl-twit:m-public-timeline))))

(defmethod initialize-instance :after ((bot microblog-bot) &key)
  "Set up the bot's state."
  (assert (source-url bot))
  (with-microblog-user bot
    (setf (previous-day bot) (floor (get-universal-time) %one-day))
    (when (= (most-recent-mention-id bot) 0)
      (setf (most-recent-mention-id bot)
	    (current-user-replies-after-id)))))

(defmethod response-for-source-request ((bot microblog-bot) mention)
  "Response for the source request."
  (format nil "@~a Hi! You can get my source here: ~a" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user mention))
	  (source-url bot)))

(defmethod respond-to-source-request ((bot microblog-bot) mention)
  "Respond to the source request."
  (let ((result (ignore-errors 
		  (cl-twit:update (response-for-source-request bot mention)))))
    (when result
      (update-id (most-recent-mention-id bot) result))))

(defmethod response-for-mention ((bot microblog-bot) mention)
  "Response for the mention object."
  (format nil "@~a Hi!" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user mention))))

(defmethod respond-to-mention ((bot microblog-bot) mention)
  "Respond to the mention object in some way."
  (let ((result (ignore-errors 
		  (cl-twit:update (response-for-mention bot mention)))))
    (when result
      (update-id (most-recent-mention-id bot) result))))

(defmethod filter-mentions ((bot microblog-bot) mentions)
  "Make sure only one mention from each user is listed."
  (remove-duplicates mentions 
		     :test #'(lambda (a b)
			       (string=
				(cl-twit:user-screen-name 
				 (cl-twit:status-user a))
				(cl-twit:user-screen-name 
				 (cl-twit:status-user b))))))

(defmethod respond-to-mentions ((bot microblog-bot))
  "Respond to new mentions since mentions were last processed."
  (assert (not (= (most-recent-mention-id bot) 0)))
  (let ((replies (cl-twit:m-replies :since-id (most-recent-mention-id bot))))
    (when replies
      (dolist (mention (filter-mentions bot replies))
	(when (not (find (cl-twit:user-screen-name 
			   (cl-twit:status-user mention))
			 (ignore-messages-from bot)
			 :test #'string=))
	  (ignore-errors 
	      (if (search "!source" (cl-twit:status-text mention))
		  (respond-to-source-request bot mention)
		  (respond-to-mention bot mention))))))))

(defmethod daily-task ((bot microblog-bot))
  "Performed every day at midnight."
  nil)

(defmethod manage-daily-task ((bot microblog-bot))
  "If it's a new day and after the appointed hour, perform the task."
  (multiple-value-bind (days seconds) (floor (get-universal-time) %one-day)
    (when (and (> days (previous-day bot))
	       (> seconds (* (daily-task-hour bot) %one-hour)))
      (ignore-errors
	(daily-task bot)
	;; Only update if successful
	;; Is this OK?
	(setf (previous-day bot) days)))))

(defmethod constant-task ((bot microblog-bot))
  "Performed every time the bot wakes up."
  nil)

(defmethod intermittent-task ((bot microblog-bot))
  "Perform some intermittent task."
  nil)

(defmethod manage-intermittent-task ((bot microblog-bot))
  "Run the task or update the wait time."  
  (if (<= (wait-remaining bot) 0)
      (progn
	(ignore-errors
	    (intermittent-task bot))
	(reset-wait bot)))
  (setf (wait-remaining bot)
	(- (wait-remaining bot)
	   *bot-sleep-time*)))

(defmethod run-bot-once ((bot microblog-bot))
  (ignore-errors
       (with-microblog-user bot
	 (manage-daily-task bot)
	 (constant-task bot)
	 (respond-to-mentions bot)
	 (manage-intermittent-task bot))))

(defmethod run-bot ((bot microblog-bot))
  "Loop forever responding to mentions & occasionaly performing periodic-task."
  (loop 
     (run-bot-once bot)
     (sleep *bot-sleep-time*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-following bot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass microblog-follower-bot (microblog-bot)
  ((most-recent-update-id :accessor most-recent-update-id 
			  :initarg :most-recent-update-id
			  :initform 0)
   (follow-id :accessor follow-id 
	      :initarg :follow-id)))

(defun current-user-posts-after-id ()
  "Get the exclusive lower bound for replies to the user to check."
  ;; Wasteful, as we only want the most recent id
  ;; If the user hasn't posted yet, start with the most recent global id
  (or (cl-twit::get-newest-id (cl-twit:m-user-timeline))
      (cl-twit::get-newest-id (cl-twit:m-public-timeline))))

(defmethod initialize-instance :after ((bot microblog-follower-bot) &key)
  "Set up the bot's state."
  (with-microblog-user bot
    (when (= (most-recent-update-id bot) 0)
      (setf (most-recent-update-id bot)
	    (current-user-posts-after-id)))))

(defmethod response-for-message ((bot microblog-follower-bot) message)
  "Response for the message object, or nil not to respond."
  nil)

(defmethod respond-to-message ((bot microblog-follower-bot) message)
  "Respond to the message object in some way."
  (let ((response (response-for-message bot message)))
    (when response
      (post response))))

(defmethod filter-messages ((bot microblog-follower-bot) messages)
  "Make sure only one mention from each user is listed."
  messages)

(defmethod respond-to-messages ((bot microblog-follower-bot))
  "Respond to new messages since messages were last processed."
  (assert (not (= (most-recent-update-id bot) 0)))
  (let ((messages 
	 (or (ignore-errors
	       (cl-twit:m-user-timeline :id (follow-id bot) 
					:since-id (most-recent-update-id bot)))
	     nil)))
    (when messages
      (dolist (message (filter-messages bot messages))
	(respond-to-message bot message))
      (setf (most-recent-update-id bot)
	    (cl-twit::get-newest-id messages)))))

(defmethod run-bot-once ((bot microblog-follower-bot))
  "Run a single iteration of the bot main loop. For running as a cron job."
    (call-next-method)
    (respond-to-messages bot))

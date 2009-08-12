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
;; Useful constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %one-hour (* 60 60)
  "One hour in seconds")
(defconstant %one-day (* %one-hour 24)
  "One day in seconds")

(defconstant %min-wait-live (* 60 60 1)
  "The default minimum time in seconds between posts used by live bots")
(defconstant %max-wait-live (* 60 60 4)
  "The default maximum time between posts used by live bots")
(defconstant %sleep-time-live (* 60 60 1)
  "The sleep time used by live bots")

(defconstant %min-wait-debug 2
  "The default minimum time in seconds between posts used by test bots")
(defconstant %max-wait-debug 4
  "The default maximum time in seconds between posts used by test bots")
(defconstant %sleep-time-debug 1
  "The sleep time used by debug bots")

(defvar %bot-sleep-time-debug 1
  "How long to wait between each check when debugging")

(defvar %bot-sleep-time-live (* 60 5)
  "How long to wait between each check when live")

(defvar *min-wait-time* %min-wait-live
  "The default minimum time in seconds between posts used by bots")

(defvar *max-wait-time* %max-wait-live
  "The default maximum time in seconds between posts used by bots")

(defvar *bot-sleep-time* %bot-sleep-time-live
  "How long to wait between each check")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling and testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *live* t)

(defun time-string ()
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (format nil "~a-~a-~a ~a:~a:~a" year month date hour minute second)))

(defun post (message)
  (if *live*
      (cl-twit:update message)
      (format t "~a - ~a~%" (time-string) message)))

(defun debug-msg (&rest args)
  (unless *live*
      (apply #'format t (concatenate 'string "~a - " (car args) "~%") 
	 (time-string) (cdr args))))

(defun set-debug ()
  (setf *live* nil)
  (setf *min-wait-time* %min-wait-debug)
  (setf *max-wait-time* %max-wait-debug)
  (setf *bot-sleep-time* %bot-sleep-time-debug))

(defun set-live ()
  (setf *live* t)
  (setf *min-wait-time* %min-wait-live)
  (setf *max-wait-time* %max-wait-live)
  (setf *bot-sleep-time* %bot-sleep-time-live))

(eval-when (:execute)
  (setf (symbol-function 'post) #'post-live)
  (setf (symbol-function 'debug-msg) #'debug-msg-live))

(defun report-error (&rest args)
  (apply #'format t args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-microblog-service (server-url from-source)
  "Set the server and the 'from..' string displayed on messages to that server."
  (debug-msg "Setting server to ~a (~a)" server-url from-source)
  (setf twit::*source* from-source)
  (setf twit::*base-url* server-url))

(defmacro update-id (place item)
  "Change the id at place for item's id if higher"
  (let ((new-id (gensym)))
    `(let ((,new-id (cl-twit::id ,item))) 
       (when (> ,new-id ,place)
	 (setf ,place ,new-id)))))

(defclass microblog-user ()
  ((user-nickname :reader user-nickname
		  :initarg :nickname)
   (user-password :reader user-password
		  :initarg :password)
   (most-recent-mention-id :accessor most-recent-mention-id 
			   :initarg :most-recent-mention-id 
			   :initform 0)))

(defmacro with-microblog-user (user &body body)
  "Log in, execture the body, log out."
  `(progn
     (cl-twit:with-session ((user-nickname ,user) 
			    (user-password ,user) 
			    :authenticatep t)
       (debug-msg "With user ~a" ,user)     
       ,@body)
       (debug-msg "Finished with user ~a" ,user)     
     ;; with-session doesn't currently do this, remove if it ever does.
     (cl-twit:logout)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic bot
;; TODO Split out into constant and intermittent bots, or tasks for a bot?
;; Tasks are probably best
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass microblog-bot (microblog-user)
  ((wait-remaining :accessor wait-remaining 
		   :initarg :wait-remaining
		   :initform 0)
   (previous-day :accessor previous-day 
		 :initarg :previous-day
		 :initform 0)
   (min-wait :accessor min-wait 
	     :initarg :min-wait
	     :initform *min-wait-time*)
   (max-wait :accessor max-wait 
	     :initarg :max-wait 
	     :initform *max-wait-time*)
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
		      (min-wait bot)))))
    (debug-msg "Resetting wait period for ~a to ~a" bot (wait-remaining bot)))

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
	    (current-user-replies-after-id))))
    (debug-msg "Initialized bot ~a previous-day ~a most-recent-mention ~a" 
	       (previous-day bot) (most-recent-mention-id bot) bot ))

(defmethod response-for-source-request ((bot microblog-bot) mention)
  "Response for the source request."
  (format nil "@~a Hi! You can get my source here: ~a" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user mention))
	  (source-url bot)))

(defmethod respond-to-source-request ((bot microblog-bot) mention)
  "Respond to the source request."
  (let ((result (handler-case
		  (post (response-for-source-request bot mention))
		  (error (err) 
		    (report-error "respond-to-source-request ~a - ~a~%" bot err)
		    nil))))
    (when result
      (update-id (most-recent-mention-id bot) result))))

(defmethod response-for-mention ((bot microblog-bot) mention)
  "Response for the mention object."
  (format nil "@~a Hi!" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user mention))))

(defmethod respond-to-mention ((bot microblog-bot) mention)
  "Respond to the mention object in some way."
  (let ((response (response-for-mention bot mention)))
    (if response
	(handler-case
	    (post response)
	  (error (err) 
	    (report-error "respond-to-mention ~a - ~a~%" bot err)
	    nil)))))

(defmethod filter-mentions ((bot microblog-bot) mentions)
  "Make sure only one mention from each user is listed."
  (remove-duplicates mentions 
		     :test #'(lambda (a b)
			       (string=
				(cl-twit:user-screen-name 
				 (cl-twit:status-user a))
				(cl-twit:user-screen-name 
				 (cl-twit:status-user b))))))

(defmethod should-ignore ((bot microblog-bot) message &optional (default t))
  "Check whether the bot should ignore the message"
  (handler-case
      (find (cl-twit:user-screen-name
	     (cl-twit:status-user message))
	    (ignore-messages-from bot)
	    :test #'string=)
    (error (err) 
      (report-error "should-ignore ~a - ~a~%" bot err)
      default)))

(defmethod new-replies ((bot microblog-bot))
  "Get any new replies for the bot's account, or nil"
  (handler-case
      (cl-twit:m-replies :since-id 
			 (most-recent-mention-id bot))
    (error (err) 
      (report-error "new-replies ~a - ~a~%" bot err)
      nil)))

(defmethod respond-to-mentions ((bot microblog-bot))
  "Respond to new mentions since mentions were last processed."
  (assert (not (= (most-recent-mention-id bot) 0)))
  (let ((replies (new-replies bot)))
    (when replies
      (dolist (mention (filter-mentions bot replies))
	(when (not (should-ignore bot mention t))
	  (handler-case
	      (if (search "!source" (cl-twit:status-text mention))
		  (respond-to-source-request bot mention)
		  (respond-to-mention bot mention))
	    (error (err) 
	      (report-error "respond-to-mentions ~a - ~a~%" bot err))))
	;; If sending fails, we still update
	(update-id (most-recent-mention-id bot) mention)))))

(defmethod daily-task ((bot microblog-bot))
  "Performed every day at midnight."
  nil)

(defmethod manage-daily-task ((bot microblog-bot))
  "If it's a new day and after the appointed hour, perform the task."
  (multiple-value-bind (days seconds) (floor (get-universal-time) %one-day)
    (when (and (> days (previous-day bot))
	       (> seconds (* (daily-task-hour bot) %one-hour)))
      (debug-msg "Running daily task for bot ~a" bot )
      (handler-case
	  (progn
	    (daily-task bot)
	    ;; Only update if successful. Is this OK?
	    (setf (previous-day bot) days))
	(error (err) 
	  (report-error "manage-daily-task ~a - ~a~%" bot err))))))

(defmethod constant-task ((bot microblog-bot))
  "Performed every time the bot wakes up."
  nil)

(defmethod intermittent-task ((bot microblog-bot))
  "Perform some intermittent task."
  nil)

(defmethod manage-intermittent-task ((bot microblog-bot))
  "Run the task or update the wait time."
  (debug-msg "Wait remaining for bot ~a is ~a" bot (wait-remaining bot))
 (if (<= (wait-remaining bot) 0)
      (handler-case
	  (progn
	    (intermittent-task bot)
	    (reset-wait bot))
	(error (err) 
	  (report-error "manage-intermittent-task ~a - ~a~%" bot err)))
      (setf (wait-remaining bot)
	    (- (wait-remaining bot)
	       *bot-sleep-time*))))

(defmethod run-bot-once ((bot microblog-bot))
  (debug-msg "Running bot once ~a" bot)
  (handler-case
      (with-microblog-user bot
	(manage-daily-task bot)
	(constant-task bot)
	(respond-to-mentions bot)
	(manage-intermittent-task bot))
    (error (err) 
      (report-error "run-bot-once ~a - ~a~%" bot err))))

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
	    (current-user-posts-after-id))))
  (debug-msg "Initialized bot ~a most-recent-mention-update ~a" 
	     bot(previous-day bot) (most-recent-update-id bot)))

(defmethod response-for-message ((bot microblog-follower-bot) message)
  "Response for the message object, or nil not to respond."
  nil)

(defmethod respond-to-message ((bot microblog-follower-bot) message)
  "Respond to the message object in some way."
  (debug-msg "Responding to message ~a" message)
  (let ((response (response-for-message bot message)))
    (when response
      (post response))))

(defmethod filter-messages ((bot microblog-follower-bot) messages)
  "Make sure only one mention from each user is listed."
  messages)

(defmethod respond-to-messages ((bot microblog-follower-bot))
  "Respond to new messages since messages were last processed."
  (assert (not (= (most-recent-update-id bot) 0)))
  (debug-msg "Responding to messages")
  (let ((messages 
	 (handler-case
	     (cl-twit:m-user-timeline :id (follow-id bot) 
				      :since-id (most-recent-update-id bot))
	   (error (err) 
	     (report-error "respond-to-messages ~a - ~a~%" bot err)
	     nil))))
    (when messages
      (debug-msg "Messages to respond to ~a" messages)
      (dolist (message (filter-messages bot messages))
	(respond-to-message bot message)
	;; If sending fails, we still update
	(update-id (most-recent-update-id bot) message))))
  (debug-msg "Most recent message after "))

(defmethod run-bot-once ((bot microblog-follower-bot))
  "Run a single iteration of the bot main loop. For running as a cron job."
    (call-next-method)
    (respond-to-messages bot))

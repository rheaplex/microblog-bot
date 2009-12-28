;; microblog-bot.lisp - Basic bot for microblogging (Twitter, Laconica).
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :microblog-bot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic bot
;; TODO Split out into constant and intermittent bots, or tasks for a bot?
;; Tasks are probably best
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %default-min-wait %one-hour
  "The default minimum time in seconds between posts used by bots")
(defconstant %default-max-wait (* %one-hour 3)
  "The default maximum time in seconds between posts used by bots")
(defconstant %default-wait-remaining 0)
(defconstant %default-daily-task-hour 0)
(defconstant %default-previous-day 0)
(defconstant %default-source-url nil)
(defconstant %default-last-handled-reply 0)
(defconstant %default-ignore-replies-from '())

(defclass microblog-bot (microblog-user)
  ((min-wait :accessor min-wait 
	     :initarg :min-wait
	     :initform %default-min-wait)
   (max-wait :accessor max-wait 
	     :initarg :max-wait 
	     :initform %default-max-wait)
   (wait-remaining :accessor wait-remaining 
		   :initarg :wait-remaining
		   :initform %default-wait-remaining)
   (daily-task-hour :accessor daily-task-hour
		    :initarg :daily-task-hour 
		    :initform %default-daily-task-hour)
   (previous-day :accessor previous-day 
		 :initarg :previous-day
		 :initform %default-previous-day)
   (source-url :accessor source-url
	       :initarg :source-url
	       :allocation :class
	       :initform %default-source-url)
   (last-handled-reply :accessor last-handled-reply 
			:initarg :last-handled-reply 
			:initform %default-last-handled-reply)
   (ignore-replies-from :accessor ignore-replies-from
			 :initarg :ignore
			 :initform %default-ignore-replies-from)))

(defmethod coherent? ((object microblog-bot))
  "Check whether the object is in a coherent state"
  (and (call-next-method)
       (> (min-wait object) 0)
       (>= (max-wait object) (min-wait object))
       (>= (wait-remaining object) 0)
       (< (wait-remaining object) (max-wait object))
       (>= 0 (daily-task-hour object) 23)
       (source-url object)
       (last-handled-reply object)))

(defmethod reset-wait ((bot microblog-bot))
  "Set the time until next execution to between min-wait..max-wait"
  (setf (wait-remaining bot)
	(+ (min-wait bot)
	   (random (- (max-wait bot)
		      (min-wait bot)))))
  (debug-msg "Resetting wait period for ~a to ~a" bot (wait-remaining bot)))

(defmethod replies-after-id ((bot microblog-bot))
  "Get the exclusive lower bound for replies to the user to check"
  (or (cl-twit::get-newest-id (cl-twit:m-user-timeline))
      (cl-twit::get-newest-id (cl-twit:m-public-timeline))))

(defmethod initialize-instance :after ((bot microblog-bot) &key)
  "Set up the bot's state"
  (assert (source-url bot))
  (with-microblog-user bot
    (setf (previous-day bot) (floor (get-universal-time) %one-day))
    (setf (last-handled-reply bot)
	  (replies-after-id bot)))
    (debug-msg "Initialized bot ~a previous-day ~a most-recent-reply ~a" 
	       bot (previous-day bot) (last-handled-reply bot)))

(defmethod response-for-source-request ((bot microblog-bot) reply)
  "Response for the source request"
  (format nil "@~a Hi! You can get my source here: ~a" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user reply))
	  (source-url bot)))

(defmethod response-for-reply ((bot microblog-bot) reply)
  "Response for the reply object"
  (format nil "@~a Hi!" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user reply))))

(defmethod response-p ((bot microblog-bot) post)
  "Check whether our post is a response."
  (search "Hi!" (cl-twit:status-text post)))

(defmethod filter-replies ((bot microblog-bot) replies)
  "Make sure only one reply from each user is listed"
  (remove-duplicates replies 
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
	    (ignore-replies-from bot)
	    :test #'string=)
    (error (err) 
      (report-error "should-ignore ~a - ~a~%" bot err)
      default)))

(defmethod new-replies ((bot microblog-bot))
  "Get any new replies for the bot's account, or nil"
  (debug-msg "new-replies after ~a" (last-handled-reply bot))
  (handler-case
      (sort (cl-twit:m-replies :since-id 
			 (last-handled-reply bot))
	    #'string< :key #'cl-twit::id)
    (error (err) 
      (report-error "new-replies ~a - ~a~%" bot err)
      nil)))

(defun source-request-p (reply)
  "Is the message a source request?"
  (search "!source" (cl-twit:status-text reply)))

(defmethod respond-to-replies ((bot microblog-bot))
  "Respond to new replies since replies were last processed"
  ;; If we've ended up with a null last-handled-reply, try to recover
  (when (not (last-handled-reply bot))
    (setf (last-handled-reply bot)
	  (replies-after-id)))
  ;; If it's still null the server is probably sad, don't respond this time
  (when (last-handled-reply bot)
    (let ((replies (filter-replies bot (new-replies bot))))
      (when replies 
	(dolist (reply replies)
	  (when (not (should-ignore bot reply t))
	    (handler-case
	     (let ((response (if (source-request-p reply)
				 (response-for-source-request bot reply)
			       (response-for-reply bot reply))))
	       (when response
		 (post response 
		       :in-reply-to-status (cl-twit::status-id reply))))
	     (error (err)
		    (report-error "respond-to-replies ~a - ~a~%" bot err)))))
	;; If any responses failed, they will be skipped
	;; This will set to null if replies are null, so ensure it's in a when 
	(setf (last-handled-reply bot)
	      (cl-twit::get-newest-id replies))))))

(defmethod daily-task ((bot microblog-bot))
  "Performed every day at the appointed hour"
  nil)

(defmethod manage-daily-task ((bot microblog-bot))
  "If it's a new day and after the appointed hour, perform the task"
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
  "Performed every time the bot wakes up"
  nil)

(defmethod intermittent-task ((bot microblog-bot))
  "Perform some intermittent task"
  nil)

(defmethod manage-intermittent-task ((bot microblog-bot))
  "Run the task or update the wait time"
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
	       %sleep-time))))

(defmethod run-bot-once ((bot microblog-bot))
  (debug-msg "Running bot once ~a" bot)
  (handler-case
      (with-microblog-user bot
	(manage-daily-task bot)
	(constant-task bot)
	(respond-to-replies bot)
	(manage-intermittent-task bot))
    (error (err) (report-error "run-bot-once ~a - ~a~%" bot err))))

(defmethod run-bot ((bot microblog-bot))
  "Loop forever responding to replies & occasionaly performing periodic-task"
  (loop 
     (run-bot-once bot)
     (sleep %sleep-time)))

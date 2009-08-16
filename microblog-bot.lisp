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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:microblog-bot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %one-hour (* 60 60)
  "One hour in seconds")

(defconstant %one-day (* 24 %one-hour)
  "One day in seconds")

(defconstant %min-wait-time %one-hour
  "The default minimum time in seconds between posts used by bots")

(defconstant %max-wait-time (* %one-hour 3)
  "The default maximum time in seconds between posts used by bots")

(defconstant %sleep-time 180
  "How long to wait between each check")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling and testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *print-debug-messages* nil
  "Whether debug messages should be printed to stdout or ignored")

(defvar *post-to-server* t
  "Whether messages should be sent to the server or just printed")

(defun time-string ()
  "Format a human-readable time"
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (format nil "~a-~a-~a ~a:~a:~a" year month date hour minute second)))

(defun debug-msg (&rest args)
  "Print to stdout if debugging, or ignore if not"
  (if *print-debug-messages*
      (apply #'format t (concatenate 'string "~a - " (car args) "~%") 
	 (time-string) (cdr args))))

(defmethod set-debug (&key (post nil) (msgs t))
  "Set the state of the library to debugging, with posting set by the keyword"
  (setf *print-debug-messages* msgs)
  (setf *post-to-server* post))

(defun set-live ()
  "Set the state of the library to live"
  (setf *print-debug-messages* nil)
  (setf *post-to-server* t))

(defun report-error (&rest args)
  "Print the error to stdout"
  (apply #'format t args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Microblog service and state utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-microblog-service (server-url from-source)
  "Set the server and the 'from..' string displayed on messages to that server"
  (debug-msg "Setting server for ~a to ~a" from-source server-url)
  (setf twit::*source* from-source)
  (setf twit::*base-url* server-url))

(defmethod post (message)
  "Post to the server if live, or just print if not"
  (when *print-debug-messages*
    (format t "~a - ~a~%" (time-string) message))
  (when *post-to-server*
      (cl-twit:m-update message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; microblog-user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass microblog-user ()
  ((user-nickname :reader user-nickname
		  :initarg :nickname)
   (user-password :reader user-password
		  :initarg :password)))

(defmacro with-microblog-user (user &body body)
  "Log in, execture the body, log out"
  `(progn
     (cl-twit:with-session ((user-nickname ,user) 
			    (user-password ,user) 
			    :authenticatep t)
       (debug-msg "With user ~a" ,user)     
       ,@body)
       (debug-msg "Finished with user ~a" ,user)     
     ;; with-session doesn't currently do this, remove if it ever does.
     (cl-twit:logout)))

(defun user-id-for-screen-name (name)
  ;; This is ambiguous. It would be better to use screen_name when supported
  (let ((user-info (or (cl-twit:m-user-show :id name)
		    (error "Can't get user info in user-id-for-screen-name"))))
    (assert user-info)
    (or (cl-twit::id user-info)
	(error "Can't get user id in user-id-for-screen-name"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic bot
;; TODO Split out into constant and intermittent bots, or tasks for a bot?
;; Tasks are probably best
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass microblog-bot (microblog-user)
  ((min-wait :accessor min-wait 
	     :initarg :min-wait
	     :initform %min-wait-time)
   (max-wait :accessor max-wait 
	     :initarg :max-wait 
	     :initform %max-wait-time)
   (wait-remaining :accessor wait-remaining 
		   :initarg :wait-remaining
		   :initform 0)
   (daily-task-hour :accessor daily-task-hour
		    :initarg :daily-task-hour 
		    :initform 0)
   (previous-day :accessor previous-day 
		 :initarg :previous-day
		 :initform 0)
   (source-url :accessor source-url
	       :initarg :source-url
	       :allocation :class
	       :initform nil)
   (last-handled-reply :accessor last-handled-reply 
			:initarg :last-handled-reply 
			:initform 0)
   (ignore-replies-from :accessor ignore-replies-from
			 :initarg :ignore
			 :initform '())))

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
  (assert (and (last-handled-reply bot)
	       (not (eq (last-handled-reply bot) 0))))
  (let ((replies (filter-replies bot (new-replies bot))))
    (when replies 
      (dolist (reply replies)
	(when (not (should-ignore bot reply t))
	  (handler-case
	    (let ((response (if (source-request-p reply)
				(response-for-source-request bot reply)
				(response-for-reply bot reply))))
	      (when response
		(post response)))
	    (error (err)
	      (report-error "respond-to-replies ~a - ~a~%" bot err)))))
      ;; If any responses failed, they will be skipped
      ;; This will set to null if replies are null, so make sure it's in a when 
      (setf (last-handled-reply bot)
	    (cl-twit::get-newest-id replies)))))

(defmethod daily-task ((bot microblog-bot))
  "Performed every day at midnight"
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-following bot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass microblog-follower-bot (microblog-bot)
  ((last-handled-post :accessor last-handled-post 
		      :initarg :last-handled-post
		      :initform 0)
   (follow-id :accessor follow-id)))

(defmethod current-user-posts-after-id ((bot microblog-follower-bot))
  "Get the exclusive lower bound for replies to the user to check"
  (or (cl-twit::get-newest-id (cl-twit:m-user-timeline))
      (cl-twit::get-newest-id (cl-twit:m-public-timeline))))

(defmethod initialize-instance :after ((bot microblog-follower-bot) 
				       &key (follow-screen-name nil))
  "Set up the bot's state" 
  (when follow-screen-name
    (setf (follow-id bot) (user-id-for-screen-name follow-screen-name)))
  (with-microblog-user bot
    (setf (last-handled-post bot)
	  (current-user-posts-after-id bot)))
  (debug-msg "Initialized bot ~a most-recent-reply-update ~a" 
	     bot (last-handled-post bot)))

(defmethod response-for-post ((bot microblog-follower-bot) post)
  "Response for the post object, or nil not to respond"
  nil)

(defmethod respond-to-post ((bot microblog-follower-bot) post)
  "Respond to the post object in some way"
  (debug-msg "Responding to post ~a" post)
  (let ((response (response-for-post bot post)))
    (when response
      (post response))))

(defmethod filter-posts ((bot microblog-follower-bot) posts)
  "Make sure only one post from each user is listed"
  posts)

(defmethod new-posts ((bot microblog-follower-bot))
  "Get posts from followed user since last checked"
  (handler-case
      (sort (cl-twit:m-user-timeline :id (follow-id bot) 
				     :since-id 
				     (last-handled-post bot))
	    #'string< :key #'cl-twit::id)
    (error (err) 
      (report-error "new-posts ~a - ~a~%" bot err)
      nil)))

(defmethod respond-to-posts ((bot microblog-follower-bot))
  "Respond to new posts since posts were last processed"
  (assert (and (last-handled-post bot)
	       (not (= (last-handled-post bot) 0))))
  (debug-msg "Responding to posts")
  (let ((posts (new-posts bot)))
    (when posts
      (debug-msg "Posts to respond to ~a" posts)
      (dolist (post (filter-posts bot posts))
	(respond-to-post bot post))
      ;; If any posts weren't processed, they will be skipped
      ;; This will set to null if posts are null, so make sure it's in a when
      (setf (last-handled-post bot)
	    (cl-twit::get-newest-id posts))))
  (debug-msg "Most recent post after ~a" (last-handled-post bot)))

(defmethod run-bot-once ((bot microblog-follower-bot))
  "Run a single iteration of the bot main loop. For running as a cron job"
    (call-next-method)
    (with-microblog-user bot
      (respond-to-posts bot)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod test-run-bot-once ((bot microblog-bot) &key (i 0) 
			      (post nil) (daily 3) (periodic 2) (fun nil) 
			      (msgs t))
  (assert (and (not (search "identi.ca" twit::*base-url*))
	       (not (search "twitter.com" twit::*base-url*))))
  (set-debug :post post :msgs msgs)
  (with-microblog-user bot
    (when fun
      (apply fun bot i))
    ;; Run the periodic task every periodic iterations
    (when (= (mod i periodic) 0)
      (setf (wait-remaining bot) 0))
    ;; Run the daily task every daily iterations
    (when (= (mod i daily) 0)
      (setf (previous-day bot) 0))
    (run-bot-once bot)))

(defmethod test-run-bot ((bot microblog-bot) iterations 
		     &key (post nil) (daily 3) (periodic 2) (fun nil) (msgs t))
  "Run the bot the given number of times, running daily & priodic tasks as set"
  (setf (daily-task-hour bot) 0)
  (dotimes (i iterations)
    (test-run-bot-once bot i :post post :daily daily :periodic periodic
		       :fun fun :msgs msgs)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for live/test configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %min-wait-live (* 60 60 1)
  "The default minimum time in seconds between posts used by live bots")

(defconstant %max-wait-live (* 60 60 4)
  "The default maximum time between posts used by live bots")

(defconstant %min-wait-debug 1
  "The default minimum time in seconds between posts used by test bots")

(defconstant %max-wait-debug 2
  "The default maximum time in seconds between posts used by test bots")

(defconstant %sleep-time-live 150
  "The sleep time used by live bots")

(defconstant %sleep-time-debug 1
  "The sleep time used by debug bots")

(defconstant %one-hour-live (* 60 60)
  "One hour in seconds for live")

(defconstant %one-day-live (* %one-hour-live 24)
  "One day in seconds for live")

(defconstant %one-hour-debug 1
  "One hour in seconds for debug")

(defconstant %one-day-debug 24
  "One day in seconds for debug")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration for live/debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It may seem strange to have days and hours as variables
;; This is so they can be set to fast-forward values for testing

(defvar *one-hour* %one-hour-live
  "One hour in seconds")

(defvar *one-day* %one-day-live
  "One day in seconds")

(defvar *min-wait-time* %min-wait-live
  "The default minimum time in seconds between posts used by bots")

(defvar *max-wait-time* %max-wait-live
  "The default maximum time in seconds between posts used by bots")

(defvar *sleep-time* %sleep-time-live
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

(defun post (message)
  "Post to the server if live, or just print if not"
  (when *post-to-server*
      (cl-twit:m-update message))
  (when *print-debug-messages*
    (format t "~a - ~a~%" (time-string) message)))

(defun debug-msg (&rest args)
  "Print to stdout if debugging, or ignore if not"
  (if *print-debug-messages*
      (apply #'format t (concatenate 'string "~a - " (car args) "~%") 
	 (time-string) (cdr args))))

(defmethod set-debug (&key (post nil))
  "Set the state of the library to debugging, with posting set by the keyword"
  (setf *print-debug-messages* t)
  (setf *post-to-server* post)
  (setf *one-hour* %one-hour-debug)
  (setf *one-day* %one-day-debug)
  (setf *min-wait-time* %min-wait-debug)
  (setf *max-wait-time* %max-wait-debug)
  (setf *sleep-time* %sleep-time-debug))

(defun set-live ()
  "Set the state of the library to live. Make sure you re-make any objects"
  (setf *print-debug-messages* nil)
  (setf *post-to-server* t)
  (setf *one-hour* %one-hour-live)
  (setf *one-day* %one-day-live)
  (setf *min-wait-time* %min-wait-live)
  (setf *max-wait-time* %max-wait-live)
  (setf *sleep-time* %sleep-time-live))

(eval-when (:execute)
  (setf (symbol-function 'post) #'post-live)
  (setf (symbol-function 'debug-msg) #'debug-msg-live))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic bot
;; TODO Split out into constant and intermittent bots, or tasks for a bot?
;; Tasks are probably best
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass microblog-bot (microblog-user)
  ((min-wait :accessor min-wait 
	     :initarg :min-wait
	     :initform *min-wait-time*)
   (max-wait :accessor max-wait 
	     :initarg :max-wait 
	     :initform *max-wait-time*)
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
   (last-handled-@reply :accessor last-handled-@reply 
			:initarg :last-handled-@reply 
			:initform 0)
   (ignore-@replies-from :accessor ignore-@replies-from 
			 :initarg :ignore
			 :initform '())))

(defmethod reset-wait ((bot microblog-bot))
  "Set the time until next execution to between min-wait..max-wait"
  (setf (wait-remaining bot)
	(+ (min-wait bot)
	   (random (- (max-wait bot)
		      (min-wait bot)))))
    (debug-msg "Resetting wait period for ~a to ~a" bot (wait-remaining bot)))

(defmethod check-@replies-after-id ((bot microblog-bot))
  "Get the exclusive lower bound for replies to the user to check"
  ;; Wasteful, as we only want the most recent id
  ;; If the user hasn't posted yet, start with the most recent global id
  (or (cl-twit::get-newest-id (cl-twit:m-replies))
      (cl-twit::get-newest-id (cl-twit:m-public-timeline))))

(defmethod initialize-instance :after ((bot microblog-bot) &key)
  "Set up the bot's state"
  (assert (source-url bot))
  (with-microblog-user bot
    (setf (previous-day bot) (floor (get-universal-time) *one-day*))
      (setf (last-handled-@reply bot)
	    (check-@replies-after-id bot)))
    (debug-msg "Initialized bot ~a previous-day ~a most-recent-@reply ~a" 
	       bot (previous-day bot) (last-handled-@reply bot)))

(defmethod response-for-source-request ((bot microblog-bot) @reply)
  "Response for the source request"
  (format nil "@~a Hi! You can get my source here: ~a" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user @reply))
	  (source-url bot)))

(defmethod response-for-@reply ((bot microblog-bot) @reply)
  "Response for the @reply object"
  (format nil "@~a Hi!" 
	  (cl-twit:user-screen-name 
	   (cl-twit:status-user @reply))))

(defmethod filter-@replies ((bot microblog-bot) @replies)
  "Make sure only one @reply from each user is listed"
  (remove-duplicates @replies 
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
	    (ignore-@replies-from bot)
	    :test #'string=)
    (error (err) 
      (report-error "should-ignore ~a - ~a~%" bot err)
      default)))

(defmethod new-@replies ((bot microblog-bot))
  "Get any new replies for the bot's account, or nil"
  (handler-case
      (sort (cl-twit:m-replies :since-id 
			 (last-handled-@reply bot))
	    #'string< :key #'cl-twit::id)
    (error (err) 
      (report-error "new-replies ~a - ~a~%" bot err)
      nil)))

(defun source-request-p (@reply)
  "Is the message a source request?"
  (search "!source" (cl-twit:status-text @reply)))

(defmethod respond-to-@replys ((bot microblog-bot))
  "Respond to new @replys since @replys were last processed"
  (assert (not (eq (last-handled-@reply bot) 0)))
  (let ((@replies (filter-@replies bot (new-@replies bot))))
    (dolist (@reply @replies)
      (when (not (should-ignore bot @reply t))
	(handler-case
	    (let ((response (if (source-request-p @reply)
				(response-for-source-request bot @reply)
				(response-for-@reply bot @reply))))
	      (when response
		(post response)))
	  (error (err)
	    (report-error "respond-to-@replys ~a - ~a~%" bot err)))))
    ;; If any responses failed, they will be skipped
    (setf (last-handled-@reply bot)
	  (cl-twit::get-newest-id @replies))))

(defmethod daily-task ((bot microblog-bot))
  "Performed every day at midnight"
  nil)

(defmethod manage-daily-task ((bot microblog-bot))
  "If it's a new day and after the appointed hour, perform the task"
  (multiple-value-bind (days seconds) (floor (get-universal-time) *one-day*)
    (when (and (> days (previous-day bot))
	       (> seconds (* (daily-task-hour bot) *one-hour*)))
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
	       *sleep-time*))))

(defmethod run-bot-once ((bot microblog-bot))
  (debug-msg "Running bot once ~a" bot)
  (handler-case
      (with-microblog-user bot
	(manage-daily-task bot)
	(constant-task bot)
	(respond-to-@replys bot)
	(manage-intermittent-task bot))
    (error (err) (report-error "run-bot-once ~a - ~a~%" bot err))))

(defmethod run-bot ((bot microblog-bot))
  "Loop forever responding to @replys & occasionaly performing periodic-task"
  (loop 
     (run-bot-once bot)
     (sleep *sleep-time*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-following bot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass microblog-follower-bot (microblog-bot)
  ((last-handled-post :accessor last-handled-post 
			  :initarg :last-handled-post
			  :initform 0)
   (follow-id :accessor follow-id 
	      :initarg :follow-id)))

(defmethod current-user-posts-after-id ((bot microblog-follower-bot))
  "Get the exclusive lower bound for replies to the user to check"
  ;; Wasteful, as we only want the most recent id
  ;; If the user hasn't posted yet, make a post and get it's id
  (let ((post-id (cl-twit::get-newest-id (cl-twit:m-user-timeline))))
   (unless post-id
     (setf post-id (parse-integer (cl-twit::id (post "Hello world")))))
   post-id))

(defmethod initialize-instance :after ((bot microblog-follower-bot) &key)
  "Set up the bot's state"
  (with-microblog-user bot
    (when (= (last-handled-post bot) 0)
      (setf (last-handled-post bot)
	    (current-user-posts-after-id bot))))
  (debug-msg "Initialized bot ~a most-recent-@reply-update ~a" 
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
  (assert (not (= (last-handled-post bot) 0)))
  (debug-msg "Responding to posts")
  (let ((posts (new-posts bot)))
    (when posts
      (debug-msg "Posts to respond to ~a" posts)
      (dolist (post (filter-posts bot posts))
	(respond-to-post bot post))
      ;; If any posts weren't processed, they will be skipped
      (setf (last-handled-post bot)
	    (cl-twit::get-newest-id posts))))
  (debug-msg "Most recent post after ~a" (last-handled-post bot)))

(defmethod run-bot-once ((bot microblog-follower-bot))
  "Run a single iteration of the bot main loop. For running as a cron job"
    (call-next-method)
    (with-microblog-user bot
      (respond-to-posts bot)))

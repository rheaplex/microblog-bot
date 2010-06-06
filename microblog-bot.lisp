;; microblog-bot.lisp - Basic bot for microblogging (Twitter, Laconica).
;; Copyright (C) 2009, 2010  Rob Myers rob@robmyers.org
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %sleep-time 180
  "How many seconds to wait between each run of the bot")

(defconstant %default-source-url nil
  "The default source url for this code (or a derivative)")
(defconstant %default-last-handled-reply 0
  "The default last reply post id handled")
(defconstant %default-ignore-replies-from '()
  "Usernames to ignore replies from")

(defclass microblog-bot (microblog-user)
  ((source-url :accessor source-url
	       :initarg :source-url
	       :allocation :class
	       :initform %default-source-url)
   (last-handled-reply :accessor last-handled-reply 
			:initarg :last-handled-reply 
			:initform %default-last-handled-reply)
   (ignore-replies-from :accessor ignore-replies-from
			 :initarg :ignore
			 :initform %default-ignore-replies-from)))

(defmethod last-handled-reply-id ((bot microblog-bot))
  "Get the exclusive lower bound for replies to the user to check"
  (or (cl-twit::get-newest-id (cl-twit:m-user-timeline))
      (cl-twit::get-newest-id (cl-twit:m-public-timeline))))

(defmethod initialize-instance :after ((bot microblog-bot) &key)
  "Set up the bot's state"
  (assert (source-url bot))
  (handler-case 
      (with-microblog-user bot    
	(setf (last-handled-reply bot)
	      (last-handled-reply-id bot)))
    (condition (the-condition) 
      (format t "Error for ~a ~%" (user-nickname bot))
      (invoke-debugger the-condition)))
  (debug-msg "Initialized bot ~a most-recent-reply ~a" 
	     bot (last-handled-reply bot)))

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
    (condition () 
      (report-error "should-ignore ~a ~a - ~a~%" (user-nickname bot) bot err)
      default)))

(defmethod new-replies ((bot microblog-bot))
  "Get any new replies for the bot's account, or nil"
  (debug-msg "new-replies after ~a" (last-handled-reply bot))
  (handler-case
      (sort (cl-twit:m-replies :since-id 
			       (last-handled-reply bot))
	    #'string< :key #'cl-twit::id)
    (condition () 
      (report-error "new-replies ~a ~a - ~a~%" (user-nickname bot) bot err)
      nil)))

(defun source-request-p (reply)
  "Is the message a source request?"
  (search "!source" (cl-twit:status-text reply)))

(defmethod respond-to-replies ((bot microblog-bot))
  "Respond to new replies since replies were last processed"
  ;; If we've ended up with a null last-handled-reply, try to recover
  (when (not (last-handled-reply bot))
    (setf (last-handled-reply bot)
	  (last-handled-reply-id bot)))
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
	     (condition ()
		    (report-error "respond-to-replies ~a ~a - ~a~%" 
				  (user-nickname bot) bot err)))))
	;; If any responses failed, they will be skipped
	;; This will set to null if replies are null, so ensure it's in a when 
	(setf (last-handled-reply bot)
	      (cl-twit::get-newest-id replies))))))

(defmethod manage-task ((bot microblog-bot))
  "Do the bot's task once."
  (respond-to-replies bot))

(defmethod run-bot-once ((bot microblog-bot))
  (debug-msg "Running bot once ~a" bot)
  (handler-case
      (with-microblog-user bot
	;; The use of :after methods ensures that this handles all subclasses
	(manage-task bot))
    (condition (the-condition) 
      (format t "Error for ~a ~a - ~a ~%" (user-nickname bot) bot the-condition)
      ;; If the error wasn't handled it's unexpected, so quit here
      (invoke-debugger the-condition))))

(defmethod run-bot ((bot microblog-bot))
  "Loop forever responding to replies & occasionaly performing periodic-task"
  (loop 
     (run-bot-once bot)
     (sleep %sleep-time)))

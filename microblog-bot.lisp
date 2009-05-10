;; microblog-bot.lisp -  Bots for microblogs (Twitter, Laconica).
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This file is part of microblog-bot
;;
;; microblog-bot is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as published 
;; by the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; microblog-bot is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License

(defpackage #:microblog-bot
  (:use #:cl #:cl-twit))

(in-package #:microblog-bot)

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

(defmacro with-microblog-user (user &body body)
  "Log in, execture the body, log out."
  `(progn
     (cl-twit:with-session ((user-nickname ,user) 
			    (user-password ,user) 
			    :authenticatep t)
       ,@body)
     ;; with-session doesn't currently do this, remove if it ever does.
     (cl-twit:logout)))

(defclass microblog-bot (microblog-user)
  ((wait-remaining :accessor wait-remaining 
		   :initarg :wait-remaining
		   :initform 0)
   (min-wait :accessor min-wait 
	     :initarg :min-wait
	     :initform (* 60 10))
   (max-wait :accessor max-wait 
	     :initarg :max-wait 
	     :initform (* 60 60 2))))

(defmethod reset-wait ((bot microblog-bot))
  "Set the bot's next wait period to between min-wait..max-wait, and wait to 0."
  (setf (wait-remaining bot)
	(+ (min-wait bot)
	   (random (- (max-wait bot)
		      (min-wait bot))))))

(defmethod initialize-instance :after ((bot microblog-bot) &key)
  "Set up the bot's state."
  ;;  (reset-wait bot)
  (with-microblog-user bot
    (when (= (most-recent-mention-id bot) 0)
      ;; Wasteful, as we only want the most recent id
      (setf (most-recent-mention-id bot)
	    (cl-twit::get-newest-id (cl-twit:m-replies))))))

(defmethod respond-to-mention ((bot microblog-bot) mention)
  "Respond to the mention object in some way."
  (cl-twit:update (format nil "@~a Hi!" 
			  (cl-twit:user-screen-name 
			   (cl-twit:status-user mention)))))

(defmethod filter-mentions ((bot microblog-bot) mentions)
  "Make sure only onemention from each user is listed."
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
	(respond-to-mention bot mention))
      (setf (most-recent-mention-id bot)
	    (cl-twit::get-newest-id replies)))))

(defmethod periodic-task ((bot microblog-bot))
  "Perform some periodic task."
  nil)

(defparameter +bot-sleep-time+ (* 60 5)
  "How long to wait between each check")

(defmethod manage-periodic-task ((bot microblog-bot))
  "Run the task or update the wait time."  
  (if (<= (wait-remaining bot) 0)
      (progn
	(periodic-task bot)
	(reset-wait bot)))
  (setf (wait-remaining bot)
	(- (wait-remaining bot)
	   +bot-sleep-time+)))

(defmethod run-bot ((bot microblog-bot))
  "Loop forever responding to mentions & occasionaly performing periodic-task."
  (loop 
     (with-microblog-user bot
       (respond-to-mentions bot)
       (manage-periodic-task bot)
       (sleep +bot-sleep-time+)))) 

#|

(defclass user-follower ()
  ((connection)
   (user-nickname)
   (most-recent-mention-id)))

(defclass trend-follower ()
  ((connection)))

(defclass search-follower ()
  ((connection)
   (words)))

(defclass hashtag-follower (search-follower)
  ())

(defclass group-follower (search-follower)
  ())



|#
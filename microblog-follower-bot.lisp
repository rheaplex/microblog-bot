;; microblog-follower-bot.lisp - A bot that follows another user's posts.
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
;; User-following bot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %default-last-handled-post 0)
(defconstant %default-follow-id nil)

(defclass microblog-follower-bot (microblog-bot)
  ((last-handled-post :accessor last-handled-post 
		      :initarg :last-handled-post
		      :initform 0)
   (follow-id :accessor follow-id)))

(defmethod coherent? ((object microblog-follower-bot))
  "Check whether the object is in a coherent state"
  (and (call-next-method)
       (last-handled-post object)
       (follow-id object) nil))

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
  (debug-msg "Responding to posts")
  ;; If we've ended up with a null last-handled-post, try to recover
  (when (not (last-handled-post bot))
    (setf (last-handled-post bot)
	  (current-user-posts-after-id)))
  ;; If it's still null the server is probably sad, don't respond this time
  (when (last-handled-post bot)
    (let ((posts (new-posts bot)))
      (when posts
      (debug-msg "Posts to respond to ~a" posts)
      (dolist (post (filter-posts bot posts))
	(respond-to-post bot post))
      ;; If any posts weren't processed, they will be skipped
      ;; This will set to null if posts are null, so make sure it's in a when
      (setf (last-handled-post bot)
	    (cl-twit::get-newest-id posts))))
    (debug-msg "Most recent post after ~a" (last-handled-post bot))))

(defmethod run-bot-once ((bot microblog-follower-bot))
  "Run a single iteration of the bot main loop. For running as a cron job"
  (call-next-method)
  (handler-case
   (with-microblog-user bot
			(respond-to-posts bot))
   (error (err) (report-error "run-bot-once ~a - ~a~%" bot err))))

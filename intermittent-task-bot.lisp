;; intermittent-task-bot.lisp - Bot that runs a task every so often.
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
  "The default minimum time in seconds between posts")
(defconstant %default-max-wait (* %one-hour 3)
  "The default maximum time in seconds between posts")
(defconstant %default-wait-remaining 0
  "The default delay until the next post")

(defclass intermittent-task-bot (microblog-bot)
  ((min-wait :accessor min-wait 
	     :initarg :min-wait
	     :initform %default-min-wait)
   (max-wait :accessor max-wait 
	     :initarg :max-wait 
	     :initform %default-max-wait)
   (wait-remaining :accessor wait-remaining 
		   :initarg :wait-remaining
		   :initform %default-wait-remaining)))

(defmethod set-next-wait ((bot intermittent-task-bot))
  "Set the time until next execution to between min-wait..max-wait"
  (setf (wait-remaining bot)
	(+ (min-wait bot)
	   (random (- (max-wait bot)
		      (min-wait bot)))))
  (debug-msg "Resetting wait period for ~a to ~a" bot (wait-remaining bot)))

(defmethod intermittent-task ((bot intermittent-task-bot))
  "Perform some intermittent task"
  nil)

(defmethod manage-intermittent-task ((bot intermittent-task-bot))
  "Run the task or update the wait time"
  (debug-msg "Wait remaining for bot ~a is ~a" bot (wait-remaining bot))
 (if (<= (wait-remaining bot) 0)
      (handler-case
	  (progn
	    (intermittent-task bot)
	    (set-next-wait bot))
	(error (err) 
	  (report-error "manage-intermittent-task ~a - ~a~%" bot err)))
      (setf (wait-remaining bot)
	    (- (wait-remaining bot)
	       %sleep-time))))

(defmethod manage-task :after ((bot intermittent-task-bot))
  "Do the bot's task once."
  (manage-intermittent-task bot))

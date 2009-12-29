;; daily-task-bot.lisp - A bot that does something once a day.
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
;; A bot that does a task once a day
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %default-daily-task-hour 0
  "The default hour of the day for the daily task")
(defconstant %default-previous-day 0
  "The default day on which the daily task last ran")

(defclass daily-task-bot (microblog-bot)
  ((daily-task-hour :accessor daily-task-hour
		    :initarg :daily-task-hour 
		    :initform %default-daily-task-hour)
   (previous-day :accessor previous-day 
		 :initarg :previous-day
		 :initform %default-previous-day)))

(defmethod initialize-instance :after ((bot daily-task-bot) &key)
  "Set up the bot's state"
  (setf (previous-day bot) (floor (get-universal-time) %one-day))
  (debug-msg "Initialized bot previous-day ~a " 
	     bot (previous-day bot)))

(defmethod daily-task ((bot daily-task-bot))
  "Performed every day at the appointed hour"
  nil)

(defmethod manage-daily-task ((bot daily-task-bot))
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

(defmethod manage-task :after ((bot daily-task-bot))
  "Do the bot's task once."
  (manage-daily-task bot))

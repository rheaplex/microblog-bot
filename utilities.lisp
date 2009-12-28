;; utilities.lisp - Basic utilities for the microblogging package.
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
;; Useful constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %one-hour (* 60 60)
  "One hour in seconds")

(defconstant %one-day (* 24 %one-hour)
  "One day in seconds")

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

(defmethod post (message &key (in-reply-to-status-id nil))
  "Post to the server if live, or just print if not"
  (when *print-debug-messages*
    (format t "~a - ~a~%" (time-string) message))
  (when *post-to-server*
      (cl-twit:m-update message :in-reply-to-status-id in-reply-to-status-id)))


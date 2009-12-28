;; testing.lisp - Test the bots.
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

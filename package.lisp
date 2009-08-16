;; packages.lisp -  The package definition(s) for microblog-bot.
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

(defpackage #:microblog-bot
  (:documentation
   "Microblog bot creation support.")
  (:use #:common-lisp #:cl-twit)
  (:export set-microblog-service
	   set-debug
	   set-live
	   report-error
	   microblog-user
	   user-nickname
	   user-password
	   with-microblog-user
	   post
	   microblog-bot
	   response-for-mention
	   response-for-source-request
	   response-for-post
	   filter-mentions
	   constant-task
	   intermittent-task
	   daily-task
	   microblog-follower-bot
	   filter-messages
	   run-bot-once
	   run-bot
	   test-run-bot-once
	   test-run-bot))

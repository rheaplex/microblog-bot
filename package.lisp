;; packages.lisp -  The package definition(s) for microblog-bot.
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This file is part of microblog-bot.
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(cl:defpackage #:microblog-bot
  (:documentation
   "Microblog bot creation support.")
  (:use #:common-lisp #:cl-twit)
  (:export set-microblog-service
	   microblog-user
	   with-microblog-user
	   microblog-bot
	   respond-to-mention
	   filter-mentions
	   periodic-task
	   run-bot))

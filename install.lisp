;; install.lisp -  Run once to install support libraries for building.
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

(require 'asdf)
(require 'asdf-install)

(asdf-install:install 'cxml-stp)
(asdf-install:install 'drakma)

(when (not (file-probe "./cl-twit.asd"))
  (sb-posix:symlink "./cl-twit/cl-twit.asd" "./cl-twit.asd"))
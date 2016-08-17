;;; init.scm --- Configuration file for Guile-Daemon

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is symlinked by "~/.config/guile-daemon/init.scm".

;;; Code:

(use-modules
 (al utils))                    ; for push!

(let ((modules (string-append (dirname (current-filename))
                              "/modules")))
  (push! modules %load-path))

(set-current-module (resolve-module '(daemon-config)))

;; init.scm ends here

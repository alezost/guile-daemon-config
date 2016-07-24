;;; message.scm --- Display message OSD

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

(define-module (daemon-config message)
  #:use-module (xosd)
  #:use-module (al utils)
  #:export (show-message-osd))

(define-delayed message-osd
  (make-osd #:timeout 5
            #:align 'right
            #:position 'top
            #:font "-*-dejavu sans-bold-r-normal-*-*-300-*-*-p-*-*"
            #:color "yellow"
            #:outline-offset 1
            #:vertical-offset 20))

(define* (show-message-osd #:optional string)
  "Show message OSD.
If STRING is specified show it in the message OSD."
  (if string
      (display-string-in-osd (message-osd) string)
      (show-osd (message-osd))))

;; message.scm ends here

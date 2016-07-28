;;; text.scm --- Display text in OSD

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

(define-module (daemon-config text)
  #:use-module (xosd)
  #:use-module (daemon-config osd)
  #:export (osd-text))

(define-osd text-osd
  (make-osd #:timeout 5
            #:align 'right
            #:position 'top
            #:font "-*-dejavu sans-bold-r-normal-*-*-300-*-*-p-*-*"
            #:color "yellow"
            #:outline-offset 1
            #:vertical-offset 20))

(define* (osd-text #:optional string)
  "Show STRING in OSD.
If STRING is not specified, show OSD with the previously displayed text."
  (if string
      (display-string-in-osd (text-osd) string)
      (show-osd (text-osd))))

;; text.scm ends here

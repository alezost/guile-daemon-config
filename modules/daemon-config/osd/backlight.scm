;;; backlight.scm --- Display screen backlight OSD

;; Copyright © 2018 Alex Kost <alezost@gmail.com>

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

(define-module (daemon-config osd backlight)
  #:use-module (ice-9 format)
  #:use-module (xosd)
  #:use-module (al backlight)
  #:use-module (daemon-config osd)
  #:use-module (daemon-config osd global)
  #:export (osd-backlight))

(define %backlight-color "#2890e8")

(define-osd backlight-osd
  (make-osd #:lines 2
            #:timeout 3
            #:align 'center
            #:position 'bottom
            #:font "-*-dejavu sans-bold-r-normal-*-*-320-*-*-p-*-*"
            #:shadow-offset 2))

(define osd-backlight
  (case-lambda
    "Show screen backlight OSD.
If called with arguments (should be strings), run 'xbacklight' with
these arguments and update the OSD accordingly."
    (()
     (show-osd (backlight-osd)))
    (args
     (apply call-xbacklight args)
     (let ((backlight (get-backlight))
           (osd       (backlight-osd)))
       (if backlight
           (let* ((backlight (inexact->exact backlight))
                  (title (format #f "Backlight: ~d%" backlight)))
             (set-osd-color! osd %backlight-color)
             (display-string-in-osd osd title)
             (display-percentage-in-osd osd backlight 1))
           (begin
             (set-osd-color! osd %color-error)
             (display-string-in-osd osd "")
             (display-string-in-osd
              osd "Oops, can't parse xbacklight output :-)" 1)))))))

;;; backlight.scm ends here

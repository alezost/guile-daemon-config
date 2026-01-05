;;; backlight.scm --- Display screen backlight OSD

;; Copyright © 2018–2026 Alex Kost <alezost@gmail.com>

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
  #:use-module (al backlight)
  #:use-module (al utils)
  #:use-module (daemon-config osd global)
  #:use-module (daemon-config osd)
  #:export (osd-backlight))

(define %backlight-color "#2890e8")

(define osd-backlight
  (case-lambda
    "Show screen backlight OSD.
If called with arguments (should be strings), run 'xbacklight' with
these arguments and update the OSD accordingly."
    (()
     (show-main-osd))
    (args
     (apply call-xbacklight args)
     (if-let ((backlight (get-backlight))
              (backlight (inexact->exact (round backlight))))
       (show-main-osd #:line0 (format #f "Backlight: ~d%" backlight)
                      #:line1 backlight
                      #:color %backlight-color)
       (show-main-osd #:line1 "Oops, can't parse xbacklight output :-)"
                      #:color %color-error)))))

;;; backlight.scm ends here

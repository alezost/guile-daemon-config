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
  #:use-module (ice-9 match)
  #:use-module (al utils)
  #:use-module (al backlight)
  #:use-module (daemon-config osd)
  #:export (osd-backlight))

(define %backlight-color "#2890e8")

(define* (output-backlight backlight #:optional (show-osd? #t))
  "Return BACKLIGHT value in Lisp format.
SHOW-OSD? defines if OSD with the backlight value should be displayed or not."
  (when show-osd?
    (show-main-osd
     #:line0 (format #f "Backlight: ~d%" backlight)
     #:line1 backlight
     #:color %backlight-color))
  (scheme->lisp backlight))

(define (osd-backlight . args)
  "Update screen backlight according to ARGS and show backlight OSD.

ARGS should be command line arguments (i.e., strings).  They have one of
the following forms:

  get,
  set VALUE: set to the specified value, see `set-backlight' for details.

Example: (osd-backlight \"set\" \"+3\")

Return the current backlight percentage (an integer from 0 to 100).
Return false value in Lisp format if backlight is not available.

If ARGS are not specified, do not show OSD, just return the backlight
percentage."
  (if (backlight-available?)
    (match args
      (()
       (output-backlight (get-backlight) #f))
      (("get")
       (output-backlight (get-backlight)))
      (("set" value)
       (output-backlight (set-backlight value)))
      (_
       (show-error-in-osd (format #f "Unknown arguments: ~a" args))))
    (show-error-in-osd "Backlight is not available")))

;;; backlight.scm ends here

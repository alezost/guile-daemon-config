;;; sound.scm --- Display sound OSD

;; Copyright © 2016–2026 Alex Kost <alezost@gmail.com>

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

(define-module (daemon-config osd sound)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (al utils)
  #:use-module (al sound)
  #:use-module (daemon-config osd)
  #:export (osd-sound))

(define %timeout-on 3)
(define %timeout-off 0)

(define* (output-sound sound #:optional (show-osd? #t))
  (if sound
    (let ((volume (sound-volume sound))
          (muted? (sound-muted? sound)))
      (when show-osd?
        (show-main-osd
         #:line0   (format #f "Sound: ~d%" volume)
         #:line1   volume
         #:color   (if muted? %color-off   %color-on)
         #:timeout (if muted? %timeout-off %timeout-on)))
      (list volume (scheme->lisp (not muted?))))
    (show-error-in-osd "Cannot obtain sound value")))

(define (osd-sound . args)
  "Update sound according to ARGS and show sound OSD.

ARGS should be command line arguments (i.e., strings).  They have one of
the following forms:

  get,
  set VALUE: set to the specified VALUE, see `set-sound' for details,
  on:        unmute (turn sound on),
  off:       mute (turn sound off),
  toggle:    toggle mute state.

Example: (osd-sound \"set\" \"+3\")

Return (VOLUME ON) list, where

  VOLUME is an integer from 0 to 100,

  ON is a boolean value in Lisp format showing if sound is on or
  off (muted).

Return false value in Lisp format in case of any error.

If ARGS are not specified, do not show OSD, just return value."
  (match args
    (()
     (output-sound (get-sound) #f))
    (("get")
     (output-sound (get-sound)))
    (("set" value)
     (output-sound (set-sound #:volume value)))
    (("on")
     (output-sound (set-sound #:mute #f)))
    (("off")
     (output-sound (set-sound #:mute #t)))
    (("toggle")
     (output-sound (set-sound #:mute 'toggle)))
    (_
     (show-error-in-osd (format #f "Unknown arguments: ~a" args)))))

;;; sound.scm ends here

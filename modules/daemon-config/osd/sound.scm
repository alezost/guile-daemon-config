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
  #:use-module (al let-macros)
  #:use-module (al sound)
  #:use-module (daemon-config osd)
  #:export (osd-sound))

(define %timeout-on 3)
(define %timeout-off 0)

(define osd-sound
  (case-lambda
    "Show sound OSD.
If called with arguments (should be strings), run 'amixer' with these
arguments and update the OSD accordingly."
    (()
     (show-main-osd))
    (amixer-args
     (if-let1 ((sound (parse-amixer-output
                       (apply call-amixer amixer-args)))
               (control (sound-control sound))
               (volume  (sound-volume  sound))
               (muted?  (sound-muted?  sound)))
       (show-main-osd #:line0   (format #f "~a: ~d%" control volume)
                      #:line1   volume
                      #:color   (if muted? %color-off   %color-on)
                      #:timeout (if muted? %timeout-off %timeout-on))
       (show-main-osd #:line1 "Oops, can't parse amixer output :-)"
                      #:color %color-error)))))

;;; sound.scm ends here

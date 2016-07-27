;;; sound.scm --- Display sound OSD

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

(define-module (daemon-config sound)
  #:use-module (ice-9 format)
  #:use-module (xosd)
  #:use-module (al sound)
  #:use-module (al utils)
  #:use-module (daemon-config global)
  #:export (osd-sound))

(define-delayed sound-osd
  (make-osd #:lines 2
            #:timeout 3
            #:align 'center
            #:position 'bottom
            #:font "-*-dejavu sans-bold-r-normal-*-*-320-*-*-p-*-*"
            #:shadow-offset 2))

(define osd-sound
  (case-lambda
    "Show sound OSD.
If called with arguments (should be strings), run 'amixer' with these
arguments and update the OSD accordingly."
    (()
     (show-osd (sound-osd)))
    (amixer-args
     (let ((sound (parse-amixer-output (apply call-amixer amixer-args)))
           (osd   (sound-osd)))
       (if sound
           (let* ((control (sound-control sound))
                  (volume  (sound-volume sound))
                  (muted?  (sound-muted? sound))
                  (color   (if muted? %color-off %color-on))
                  (title   (format #f "~a: ~d%" control volume)))
             (set-osd-color! osd color)
             (display-string-in-osd osd title)
             (display-percentage-in-osd osd volume 1))
           (begin
             (set-osd-color! osd %color-error)
             (display-string-in-osd osd "")
             (display-string-in-osd
              osd "Oops, can't parse amixer output :-)" 1)))))))

;; sound.scm ends here

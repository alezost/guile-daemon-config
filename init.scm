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

(use-modules
 (ice-9 format)
 (xosd)
 (al sound)
 (al utils))

(define %color-on "#23B13E")
(define %color-off "#E74F35")
(define %color-error "orange")

(define-delayed message-osd
  (make-osd #:timeout 5
            #:align 'right
            #:position 'top
            #:font "-*-dejavu sans-bold-r-normal-*-*-300-*-*-p-*-*"
            #:color "yellow"
            #:outline-offset 1
            #:vertical-offset 20))

(define-delayed sound-osd
  (make-osd #:lines 2
            #:timeout 3
            #:align 'center
            #:position 'bottom
            #:font "-*-dejavu sans-bold-r-normal-*-*-320-*-*-p-*-*"
            #:shadow-offset 2))

(define-delayed clock-osd
  (make-osd #:position 'middle
            #:align 'center
            #:font "-*-ubuntu-bold-r-normal-*-*-3800-*-*-p-*-*"
            #:color "#BFBF00"
            #:shadow-offset 3))

(define* (show-message-osd #:optional string)
  "Show message OSD.
If STRING is specified show it in the message OSD."
  (if string
      (display-string-in-osd (message-osd) string)
      (show-osd (message-osd))))

(define show-sound-osd
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

(define toggle-clock-osd
  (let ((thread #f))
    (lambda ()
      "Show/hide clock OSD."
      (define osd (clock-osd))

      (define (display-clock)
        (display-string-in-osd
         osd (strftime "%H:%M" (localtime (current-time))))
        (sleep 1)
        (display-clock))

      (if (and thread (not (thread-exited? thread)))
          (cancel-thread thread)
          (begin
            (set! thread (call-with-new-thread display-clock))
            (set-thread-cleanup! thread
                                 (lambda () (hide-osd osd))))))))

;; init.scm ends here

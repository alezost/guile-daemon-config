;;; clock.scm --- Display clock OSD

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

(define-module (daemon-config clock)
  #:use-module (xosd)
  #:use-module (daemon-config osd)
  #:export (toggle-clock-osd))

(define-osd clock-osd
  (make-osd #:position 'middle
            #:align 'center
            #:font "-*-ubuntu-bold-r-normal-*-*-3800-*-*-p-*-*"
            #:color "#BFBF00"
            #:shadow-offset 3))

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

;;; clock.scm ends here

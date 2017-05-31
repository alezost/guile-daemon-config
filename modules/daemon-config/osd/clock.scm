;;; clock.scm --- Display clock OSD

;; Copyright © 2016, 2017 Alex Kost <alezost@gmail.com>

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

(define-module (daemon-config osd clock)
  #:use-module (ice-9 threads)
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
          ;; FIXME It is better to hide osd inside a thread cleanup
          ;; handler, which can be created with `dynamic-wind' according to
          ;; <http://git.savannah.gnu.org/cgit/guile.git/commit/?id=eeeee3297b8d4cb0717ee3b9ae5068b4f0b7f118>.
          ;; But I don't know how to do it :-(
          (begin (cancel-thread thread)
                 (hide-osd osd))
          (set! thread (call-with-new-thread display-clock))))))

;;; clock.scm ends here

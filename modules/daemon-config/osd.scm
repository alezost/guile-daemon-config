;;; osd.scm --- Common utils and settings used by the other OSD modules

;; Copyright © 2026 Alex Kost <alezost@gmail.com>

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

(define-module (daemon-config osd)
  #:use-module (ice-9 match)
  #:use-module (xosd)
  #:use-module (al utils)
  #:use-module (al osd)
  #:export (%color-on
            %color-off
            %color-error
            show-main-osd
            show-error-in-osd))

(define %color-on "#23B13E")
(define %color-off "#E74F35")
(define %color-error "orange")

(define-osd main-osd
  #:lines 2
  #:timeout 3
  #:align 'center
  #:position 'bottom
  #:font "-*-dejavu sans-bold-r-normal-*-*-320-*-*-p-*-*"
  #:shadow-offset 2)

(define show-main-osd
  (let ((prev-osd-args #f))
    (case-lambda*
      "Show main OSD.
If called without arguments, just show the OSD.

LINE0 / LINE1 can be a string (for `display-string-in-osd') or a
number (for `display-percentage-in-osd').

The rest keyword arguments, ARGS, are passed to `set-osd!' procedure."
      (()
       (show-osd (main-osd)))
      ((#:key (line0 "") (line1 "") #:allow-other-keys #:rest args)
       (let* ((osd (main-osd))
              (show (lambda (str-or-num line)
                      (match str-or-num
                        ((? string? str)
                         (display-string-in-osd osd str line))
                        ((? number? num)
                         (display-percentage-in-osd osd num line))
                        (else
                         (display-string-in-osd
                          osd
                          (format #f "'~a' must be a number/string"
                                  str-or-num)
                          line)))))
              (osd-args (remove-keywords args #:line0 #:line1)))
         (unless (and prev-osd-args
                      (equal? prev-osd-args osd-args))
           (set! prev-osd-args osd-args)
           (apply set-osd! osd osd-args))
         (show line0 0)
         (show line1 1))))))

(define (show-error-in-osd string)
  "Show STRING in osd using `%color-error' color.
Return false value in Lisp format."
  (show-main-osd #:line1 string
                 #:color %color-error)
  ;; Use Lisp format because this output is used by StumpWM.
  (scheme->lisp #f))

;;; osd.scm ends here

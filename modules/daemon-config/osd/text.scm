;;; text.scm --- Display text in OSD

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

(define-module (daemon-config osd text)
  #:use-module (ice-9 match)
  #:use-module (xosd)
  #:use-module (al utils)
  #:use-module (al osd)
  #:export (osd-text))

(define-values (text-osd
                hide-text-osds)
  (let ((osds '()))
    (values
     (memoize
      (lambda (number-of-lines)
        (let ((osd (make-osd
                    #:lines number-of-lines
                    #:timeout 5
                    #:align 'right
                    #:position 'top
                    #:font "-*-dejavu sans-bold-r-normal-*-*-300-*-*-p-*-*"
                    #:color "yellow"
                    #:outline-offset 1
                    #:vertical-offset 20)))
          (register-osd osd)
          (push! osd osds)
          osd)))
     (lambda line-numbers
       "Hide all text OSDs that do not have one of the LINE-NUMBERS."
       (for-each (lambda (osd)
                   (unless (memv (osd-number-of-lines osd)
                                 line-numbers)
                       (hide-osd osd)))
                 osds)))))

(define (display-pause-in-osd osd line-number pause)
  (let loop ((seconds pause))
    (when (> seconds 0)
      (display-string-in-osd osd (make-string seconds #\.) line-number)
      (sleep 1)
      (loop (1- seconds)))))

(define* (display-strings-in-osd strings pause)
  (let* ((lines (length strings))
         (osd   (text-osd lines)))
    (hide-text-osds lines)
    (let loop ((strings strings)
               (line 0))
      (unless (null? strings)
        (display-string-in-osd osd (car strings) line)
        (let ((rest (cdr strings))
              (next-line (1+ line)))
          (unless (null? rest)
            (display-pause-in-osd osd next-line pause))
          (loop rest next-line))))))

(define* (osd-text #:key (pause 0) #:rest strings)
  "Show STRINGS in OSD on separate lines.
PAUSE is the number of seconds to pause between displaying STRINGS.
If STRINGS are not specified, show OSD with the previously displayed string.
If a single string is specified, it may contain newlines."
  (match (remove-keywords strings)
    (()
     (show-osd (text-osd 1)))
    ((string)
     (display-strings-in-osd (string-split string #\newline) pause))
    (strings
     (display-strings-in-osd strings pause))))

;;; text.scm ends here

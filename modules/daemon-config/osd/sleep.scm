;;; sleep.scm --- Set up "sleep" with OSD

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

;;; Commentary:

;; This module allows to set `sleep-command' which will be started after
;; waiting for some sleep time, that can be displayed and controlled
;; with `osd-sleep' procedure.  For example: (osd-sleep "+7") to add 7
;; minutes to the current sleep time, (osd-sleep 9) to set it to 9
;; minutes, etc.
;;
;; 'time' in the code of this module always means "the number of
;; minutes".

;;; Code:

(define-module (daemon-config osd sleep)
  #:use-module (ice-9 threads)
  #:use-module (xosd)
  #:use-module (daemon-config osd)
  #:use-module (daemon-config osd global)
  #:export (osd-sleep
            sleep-osd
            sleep-command))

(define-osd sleep-osd
  (make-osd #:position 'top
            #:align 'left
            #:timeout 5
            #:font "-*-liberation sans-bold-r-normal-*-*-600-*-*-p-*-*"
            #:shadow-offset 2))

;; Not using paramater to make it possible to change sleep command when
;; sleeping process is on.
(define sleep-command
  ;; 'turnoff' is my shell script:
  ;; <https://github.com/alezost/shell-config/blob/master/scripts/turnoff>
  (let ((command "turnoff --all"))
    (lambda* (#:optional new-command)
      "Return the current sleep command.
If NEW-COMMAND is specified, set the current sleep command to it."
      (when new-command
        (set! command new-command))
      command)))

(define (showtime? time)
  "Return #t if TIME is one of those number of minutes when the OSD
should be displayed."
  (->bool (memv time '(1 5 10))))

(define* (calculate-time time-value #:optional (base-time 0))
  "Calculate and return the number of minutes.

TIME-VALUE can be a number or a string.  If it is a number, return this
number of minutes.  If it is a string, it should be a string with a
number optionally starting with '-' or '+'.  This string denotes the
number of minutes to set, add or subtract to/from BASE-TIME: like '18',
'+3' '-7'.

BASE-TIME is the number of minutes used for adding/subtraction.

Return 0 if the result is less than 0."
  (define time
    (if (number? time-value)
        time-value
        (case (string-ref time-value 0)
          ((#\+) (+ base-time (string->number (substring time-value 1))))
          ((#\-) (- base-time (string->number (substring time-value 1))))
          (else (string->number time-value)))))
  (max 0 time))

(define-values (countdown
                osd-sleep)
  (let ((time 0)
        (thread #f))
    (values
     (lambda ()
       "Call 'sleep-command' after waiting for the current sleep time."
       (sleep 60)
       (set! time (1- time))
       (when (showtime? time)
         (osd-sleep))
       (if (> time 0)
           (countdown)
           (begin
             ;; Last chance to avoid sleeping.
             (display-string-in-osd (sleep-osd) "Sleeping...")
             (sleep 3)
             (if (> time 0)
                 (countdown)
                 (system (sleep-command))))))
     (case-lambda
       "Show sleep OSD.
If VALUE is specified, set sleep time to this VALUE.
See 'calculate-time' for details."
       (()
        (set-osd-color! (sleep-osd)
                        (if (zero? time) %color-off %color-on))
        (display-string-in-osd
         (sleep-osd)
         (string-append "Sleep: " (number->string time))))
       ((value)
        (let ((new-time (calculate-time value time)))
          (if (zero? new-time)
              (when (and thread (not (thread-exited? thread)))
                (cancel-thread thread))
              (when (or (not thread) (thread-exited? thread))
                (set! thread (call-with-new-thread countdown))))
          (set! time new-time)
          (osd-sleep)))))))

;;; sleep.scm ends here

;;; keys.scm --- Handle remote control keys

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

(define-module (daemon-config lirc keys)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-26)
  #:use-module (xosd)
  #:use-module (daemon-config osd clock)
  #:use-module (daemon-config osd text)
  #:use-module (daemon-config osd sleep)
  #:use-module (daemon-config osd sound)
  #:export (handle-rc-key))

(define rc-mode
  (let ((mode "main"))
    (lambda* (#:optional new-mode)
      "Return the current remote control mode.
If NEW-MODE is specified, set the current mode to it."
      (when new-mode
        (set! mode new-mode))
      mode)))

(define* (rc-mode-show #:optional new-mode)
  "Call '(rc-mode NEW-MODE)' and show the mode in OSD."
  (osd-text (string-append "Mode: " (rc-mode new-mode))))

(define* (set-sound value #:optional (control "Master"))
  "Set sound VALUE of the simple CONTROL and show it in OSD."
  (osd-sound "sset" control value))

(define* (show-unbound-key key #:optional (mode (rc-mode)))
  "Display OSD message about KEY unbound in MODE."
  (osd-text (string-append "Unbound key: " key)
            (string-append "(mode: " mode ")")))

(define number-key
  (let ((rx (make-regexp "KEY_([0-9])")))
    (lambda (key)
      "Return KEY's number if remote control KEY is a number key.
Return #f if KEY is not a number key."
      (and=> (regexp-exec rx key)
             (lambda (m)
               (string->number (match:substring m 1)))))))

(define (tvtime-command . args)
  (apply system* "tvtime-command" args))

(define (emms-command . args)
  (apply system*
         "emacsclient" "--socket-name=server-emms" "--eval" args))

(define (handle-rc-key key)
  "Run some command for the remote control KEY."
  (define key?  (cut string=? <> key))
  (define mode? (cut string=? <> (rc-mode)))

  (cond
   ((mode? "show-key")
    (if (key? "KEY_MODE")
        (rc-mode-show "main")
        (osd-text key)))

   ;; Global keys.
   ((key? "KEY_INFO")
    (toggle-clock-osd))
   ((key? "KEY_CAMERA")
    (system "stumpish -e al/toggle-root < /dev/null"))
   ((key? "KEY_SLEEP")
    (if (mode? "sleep")
        (hide-osd (sleep-osd))
        (let ((mode (rc-mode)))
          (rc-mode "sleep")
          (osd-sleep)
          (call-with-new-thread
           (lambda ()
             (wait-while-osd-displayed (sleep-osd))
             (rc-mode-show mode))))))

   ((mode? "sleep")
    (cond
     ((key? "KEY_0")
      (osd-sleep 0))
     ((key? "KEY_1")
      (osd-sleep "+1"))
     ((key? "KEY_7")
      (osd-sleep "-1"))
     ((key? "KEY_2")
      (osd-sleep "+3"))
     ((key? "KEY_8")
      (osd-sleep "-3"))
     ((key? "KEY_3")
      (osd-sleep "+10"))
     ((key? "KEY_9")
      (osd-sleep "-10"))
     ((key? "KEY_CHANNELDOWN")
      (osd-sleep "-10"))
     ((key? "KEY_CHANNELUP")
      (osd-sleep "+10"))
     (else
      (hide-osd (sleep-osd)))))

   ((mode? "tv")
    (cond
     ((key? "KEY_MODE")
      (rc-mode-show "show-key"))
     ((key? "KEY_POWER")
      (tvtime-command "QUIT")
      (rc-mode-show "main"))
     ((key? "KEY_CHANNELDOWN")
      (set-sound "3%-" "Line"))
     ((key? "KEY_CHANNELUP")
      (set-sound "3%+" "Line"))
     ((key? "KEY_MUTE")
      (set-sound "toggle" "Line"))
     ((key? "KEY_RED")
      (tvtime-command "SHOW_EPG"))
     ((key? "KEY_PLAYPAUSE")
      (tvtime-command "MENU_UP"))
     ((key? "KEY_GREEN")
      (tvtime-command "MENU_DOWN"))
     ((key? "KEY_OK")
      (tvtime-command "DISPLAY_INFO"))
     ((key? "KEY_SCREEN")
      (tvtime-command "TOGGLE_ASPECT"))
     ((key? "KEY_ZOOM")
      (tvtime-command "TOGGLE_FULLSCREEN"))
     ((key? "KEY_AGAIN")
      (tvtime-command "CHANNEL_PREV"))
     ((key? "KEY_VOLUMEDOWN")
      (tvtime-command "CHANNEL_DOWN"))
     ((key? "KEY_VOLUMEUP")
      (tvtime-command "CHANNEL_UP"))
     ((number-key key) =>
      (lambda (number)
        (tvtime-command
         (string-append "CHANNEL_" (number->string number)))))
     (else
      (show-unbound-key key))))

   ;; Anything else is the "main" mode.
   (else
    (cond
     ((key? "KEY_MODE")
      (rc-mode-show "tv"))
     ((key? "KEY_POWER")
      (call-with-new-thread
       (lambda () (system* "toggle-tvtime")))
      (rc-mode-show "tv"))
     ((key? "KEY_CHANNELDOWN")
      (set-sound "3%-"))
     ((key? "KEY_CHANNELUP")
      (set-sound "3%+"))
     ((key? "KEY_MUTE")
      (set-sound "toggle"))
     ((key? "KEY_PLAYPAUSE")
      (emms-command "(emms-pause)"))
     ((key? "KEY_VOLUMEDOWN")
      (emms-command "(emms-previous)"))
     ((key? "KEY_VOLUMEUP")
      (emms-command "(emms-next)"))
     ((key? "KEY_RECORD")
      (emms-command "(al/emms-seek-backward 60)"))
     ((key? "KEY_STOP")
      (emms-command "(al/emms-seek-forward 60)"))
     ((key? "KEY_TEXT")
      (emms-command "(al/emms-seek-backward 10)"))
     ((key? "KEY_VIDEO")
      (emms-command "(al/emms-seek-forward 10)"))
     ((key? "KEY_OK")
      (emms-command "(al/emms-mpv-show-progress)"))
     ((key? "KEY_ZOOM")
      (emms-command "(al/emms-mpv-toggle-fullscreen)"))
     (else
      (show-unbound-key key))))))

;;; keys.scm ends here

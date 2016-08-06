;;; client.scm --- Client for LIRC daemon

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

(define-module (daemon-config lirc client)
  #:use-module (al lirc)
  #:export (lirc-client-connect
            lirc-client-disconnect
            lirc-client-reconnect))

(define (key-handler key)
  ((module-ref (resolve-interface '(daemon-config lirc keys))
               'handle-rc-key)
   key))

(define-values (lirc-client-connect
                lirc-client-disconnect)
  (let ((connection #f))
    (values
     (lambda ()
       (set! connection
             ;; Not using 'handle-rc-key' directly to make it possible
             ;; to re-evaluate it during on-the-fly hacking, and also to
             ;; avoid loading (daemon-config lirc keys) module and its
             ;; dependencies on start-up.
             (lirc-connect key-handler)))
     (lambda ()
       (when (lirc-connection? connection)
         (lirc-disconnect connection))))))

(define (lirc-client-reconnect)
  (lirc-client-disconnect)
  (lirc-client-connect))

;;; client.scm ends here

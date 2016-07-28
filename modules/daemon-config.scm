;;; daemon-config.scm --- Autoload procedures for my Guile-Daemon config

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

(define-module (daemon-config)
  #:autoload (daemon-config clock) (toggle-clock-osd)
  #:autoload (daemon-config text)  (osd-text)
  #:autoload (daemon-config sleep) (osd-sleep
                                    sleep-command)
  #:autoload (daemon-config sound) (osd-sound))

;; daemon-config.scm ends here

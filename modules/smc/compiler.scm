;;; compiler.scm -- Guile-SMC state machine compiler.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; The compiler produces a Scheme code from a <fsm> instance.


;;; Code:

(define-module (smc compiler)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (smc core log)
  #:use-module (smc core state)
  #:use-module (smc core set)
  #:use-module (smc version)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc compiler guile)
  #:export (fsm-compile))



(define* (fsm-compile fsm
                      #:key
                      (fsm-name 'custom-fsm)
                      (fsm-module    #f)
                      (extra-modules '())
                      (standalone-mode? #f)
                      (output-port (current-output-port)))

  (when (fsm-event-source-anonymous? fsm)
    (let ((error-message
           "Cannot compile the FSM as the event source is set to an anonymous procedure"))
      (log-error "~a: ~a" error-message fsm)
      (error error-message fsm)))

  (write-header output-port)

  (when (fsm-parent fsm)
    (write-parent-fsm-info fsm output-port))

  (form-feed output-port)
  (let ((class-name (string->symbol (format #f "<~a>" fsm-name))))
    (if fsm-module
        (write-module fsm-module
                      #:extra-modules    extra-modules
                      #:class-name       class-name
                      #:port             output-port
                      #:standalone-mode? standalone-mode?)
        (write-use-modules extra-modules output-port))

    (newline output-port)

    (form-feed output-port)
    (write-transition-table fsm output-port)

    (newline output-port)

    (form-feed output-port)
    (write-define-class class-name output-port)

    (newline output-port)

    (form-feed output-port)
    (write-initialize fsm class-name output-port)

    (newline output-port)))

;;; compiler.scm ends here.

;;; compiler.scm -- Guile-SMC state machine compiler.

;; Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (smc config)
  #:use-module (smc compiler guile-common)
  #:use-module (smc compiler guile)
  #:use-module (smc compiler guile-standalone)
  #:export (fsm-compile))


(define* (fsm-compile/guile fsm
                            #:key
                            fsm-name
                            fsm-module
                            extra-modules
                            output-port)
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
                      #:standalone-mode? #f)

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

(define* (fsm-compile/guile-standalone-copy fsm
                                            #:key
                                            fsm-name
                                            fsm-module
                                            extra-modules
                                            output-port)
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
                      #:standalone-mode? #t)

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

        (newline output-port)
        (write-footer fsm-name output-port)))

(define* (fsm-compile/guile-standalone fsm
                                       #:key
                                       fsm-name
                                       fsm-module
                                       extra-modules
                                       output-port
                                       (modules-path %guile-smc-modules-directory)
                                       (optimize? #t))
  (write-header output-port)

  (when (fsm-parent fsm)
    (write-parent-fsm-info fsm output-port))

  (pretty-print (fsm-define-module fsm
                                   fsm-name
                                   fsm-module
                                   #:extra-modules extra-modules)
                output-port
                #:display? #f)
  (newline output-port)
  (form-feed output-port)

  (pretty-print (fsm-get-class-code fsm-name)
                output-port
                #:display? #f)

  (newline output-port)
  (form-feed output-port)

  (let ((context-code (fsm-get-context-code modules-path))
        (fsm-code     (fsm->standalone-code fsm fsm-name)))
    (for-each (lambda (sexp)
                (if (equal? (car sexp) 'define-module)
                    (let ((module (cadr sexp)))
                      (write-section-header (format #f "From ~a" module)
                                            output-port))
                    (begin
                      (pretty-print sexp output-port #:display? #f)
                      (newline))))
              (if optimize?
                  (prune-unused-definitions
                   (prune-unused-definitions context-code (list fsm-code))
                   (list fsm-code))
                  context-code))
    (newline output-port)
    (form-feed output-port)
    (pretty-print fsm-code
                  output-port
                  #:display? #f))

  (newline output-port)
  (write-footer fsm-name output-port))


(define* (fsm-compile fsm
                      #:key
                      (fsm-name      'custom-fsm)
                      (fsm-module    #f)
                      (extra-modules '())
                      (modules-path  %guile-smc-modules-directory)
                      (target        'guile)
                      (output-port   (current-output-port)))
  "Compile an FSM for the specified TARGET and print the result to an
OUTPUT-PORT.  Use an FSM-NAME as the name of the output FSM."

  (when (fsm-event-source-anonymous? fsm)
    (let ((error-message
           "Cannot compile the FSM as the event source is set to an anonymous procedure"))
      (log-error "~a: ~a" error-message fsm)
      (error error-message fsm)))

  (case target
    ((guile)
     (fsm-compile/guile fsm
                        #:fsm-name      fsm-name
                        #:fsm-module    fsm-module
                        #:extra-modules extra-modules
                        #:output-port   output-port))
    ((guile-standalone)
     (fsm-compile/guile-standalone fsm
                                   #:modules-path  modules-path
                                   #:fsm-name      fsm-name
                                   #:fsm-module    fsm-module
                                   #:extra-modules extra-modules
                                   #:output-port   output-port))
    ((guile-standalone-copy)
     (fsm-compile/guile-standalone-copy fsm
                                        #:fsm-name      fsm-name
                                        #:fsm-module    fsm-module
                                        #:extra-modules extra-modules
                                        #:output-port   output-port))
    (else
     (error "Unknown compilation target" target))))



;;; compiler.scm ends here.

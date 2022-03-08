;;; guile-standalone.scm -- Guile-SMC state machine compiler procedures.

;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; The procedures in this module convert an FSM instance to a Scheme code that
;; does not depend on Guile-SMC.


;;; Code:

(define-module (smc compiler guile-standalone)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (smc core common)
  #:use-module (smc core state)
  #:use-module (smc core transition)
  #:use-module (smc core log)
  #:use-module (smc version)
  #:use-module (smc fsm)
  #:export (fsm->standalone-code
            fsm-define-module
            fsm-get-context-code
            state->standalone-code
            fsm-transition-table->standalone-code))



(define-method-with-docs (state->standalone-code (fsm <fsm>) (state <list>))
  "Convert a STATE to a plain Scheme 'define' that does not depend on
Guile-SMC."
  (let ((name         (state:name state))
        (description  (state:description state))
        (event-source (procedure-name (if (state:event-source state)
                                          (state:event-source state)
                                          (fsm-event-source fsm))))
        (entry-action (state:entry-action state))
        (exit-action  (state:exit-action  state))
        (transitions  (state:transitions  state)))
    `(define (,name context)
       ,(if description
            description
            "")
       (let ((event ,(list event-source
                           (if (or (not entry-action) (null? entry-action))
                               'context
                               (list (procedure-name entry-action) 'context)))))
         (cond
          ,@(map (lambda (transition)
                   (let ((tguard (transition:guard      transition))
                         (action (transition:action     transition))
                         (nstate (and (transition:next-state transition)
                                      (state-name (transition:next-state transition)))))
                     (list (list (procedure-name tguard) 'context 'event)
                           `(let ,(list (list 'context
                                              (list (procedure-name action)
                                                    'context
                                                    'event)))
                              ,@(if nstate
                                    (list
                                      (list 'log-debug
                                            "[~a] -> [~a]"
                                            name
                                            nstate)
                                      (if (or (not exit-action) (null? exit-action))
                                          (list nstate 'context)
                                          (list nstate (list (procedure-name exit-action)
                                                             'context))))
                                    (list
                                     (list 'log-debug
                                           "[~a] -> [*]"
                                           name)
                                     (if (or (not exit-action)
                                             (null? exit-action))
                                         'context
                                         (list (procedure-name exit-action)
                                               'context))))))))
                 transitions))))))

(define-method-with-docs (fsm-transition-table->standalone-code (fsm <fsm>))
  "Convert a @var{fsm} transition table to a list of Scheme procedures that do
not depend on Guile-SMC.."
  (let* ((table           (fsm-transition-table fsm))
         (transition-list (hash-table->transition-list table)))
    (map (lambda (state)
           (state->standalone-code fsm state))
         transition-list)))

(define (string-drop-both str)
  (string-drop-right (string-drop str 1) 1))

(define* (fsm-define-module fsm
                            module
                            #:key
                            extra-modules)
  (let* ((cname (string-drop-both (symbol->string (class-name (class-of fsm)))))
         (proc-name (string->symbol (string-append "run-" cname))))
    (let loop ((lst `(define-module ,module
                       #:use-module (oop goops)
                       #:use-module (logging logger)
                       #:use-module (scheme documentation)))
               (em  extra-modules))
      (if (or (not em) (null? em))
          (append lst `(#:export (,proc-name)))
          (loop (append lst `(#:use-module ,(car em)))
                (cdr em))))))

(define (fsm-get-context-code guile-smc-modules-path)
  "Read the Guile-SCM context from the GUILE-SMC-MODULES-PATH and return the
code as a list."
  (define (read-module path module-name)
    (let ((port (open-input-file (string-append path "/" module-name))))
      ;; Skip the 'define-module' part.
      (read port)
      (let loop ((sexp   (read port))
                 (result '()))
        (if (not (eof-object? sexp))
            (loop (read port)
                  (cons sexp result))
            (begin
              (close port)
              (reverse result))))))

  (let ((core-path    (string-append guile-smc-modules-path "core"))
        (context-path (string-append guile-smc-modules-path "context")))
    `(,@(read-module core-path "common.scm")
      ,@(read-module core-path "log.scm")
      ,@(read-module context-path "context.scm")
      ,@(read-module context-path "char-context.scm"))))

(define-method-with-docs (fsm->standalone-code (fsm <fsm>))
  "Convert an @var{fsm} to a procedure that does not depend on Guile-SMC."
  (let* ((cname (string-drop-both (symbol->string (class-name (class-of fsm)))))
         (proc-name (string->symbol (string-append "run-" cname))))
    `(define (,proc-name context)
       ,(if (fsm-description fsm)
            (fsm-description fsm)
            "")
       ,@(fsm-transition-table->standalone-code fsm)
       ,(list (state-name (fsm-current-state fsm)) 'context))))

;;; guile-standalone.scm ends here.

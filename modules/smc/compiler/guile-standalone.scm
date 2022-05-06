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
            fsm-get-class-code
            fsm-get-context-code
            state->standalone-code
            prune-unused-definitions
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
                                    (if (equal? nstate name)
                                        (list
                                         (if (or (not exit-action) (null? exit-action))
                                             (list nstate 'context)
                                             (list nstate (list (procedure-name exit-action)
                                                                'context))))
                                        (list
                                         (list 'log-debug
                                               "[~a] -> [~a]"
                                               (list 'quote name)
                                               (list 'quote nstate))
                                         (if (or (not exit-action) (null? exit-action))
                                             (list nstate 'context)
                                             (list nstate (list (procedure-name exit-action)
                                                                'context)))))
                                    (list
                                     (list 'log-debug
                                           "[~a] -> [*]"
                                           (list 'quote name))
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
  "Drop one symbol from both left and right parts of a STR."
  (string-drop-right (string-drop str 1) 1))

(define* (fsm-define-module fsm
                            fsm-name
                            module
                            #:key
                            extra-modules)
  (let ((cname (string->symbol (format #f "<~a>" fsm-name))))
    (let loop ((lst `(define-module ,module
                       #:use-module (oop goops)
                       #:use-module (logging logger)
                       #:use-module (logging rotating-log)
                       #:use-module (scheme documentation)
                       #:use-module (ice-9 textual-ports)))
               (em  extra-modules))
      (if (or (not em) (null? em))
          (append lst `(#:export (,cname
                                  fsm-run!
                                  fsm-debug-mode?
                                  fsm-debug-mode-set!)))
          (loop (append lst `(#:use-module ,(car em)))
                (cdr em))))))

(define (tree-contains? root elem)
  "Check if a ROOT contains an ELEM."
  (cond
   ((pair? root)
    (or (tree-contains? (car root) elem)
        (tree-contains? (cdr root) elem)))
   (else
    (equal? root elem))))

(define (prune-unused-definitions definitions hardwired-definitions)
  "Remove all the definitions from the DEFINITIONS list that are not used
neither in the DEFINITIONS nor HARDWIRED-DEFINITIONS lists."
  (let main-loop ((defs   definitions)
                  (result '()))
    (if (null? defs)
        result
        (let* ((def (car defs))
               (sym (if (pair? (cadr def))
                        (caadr def)
                        (cadr def))))
          (if (or (tree-contains? (cdr defs) sym)
                  (tree-contains? result sym)
                  (tree-contains? hardwired-definitions sym))
              (main-loop (cdr defs)
                         (append result (list def)))
              (main-loop (cdr defs)
                         result))))))

(define* (fsm-get-context-code guile-smc-modules-path #:key (skip-define-module? #t))
  "Read the Guile-SCM context from the GUILE-SMC-MODULES-PATH and return the
code as a list."
  (define (read-module path module-name)
    (let ((port (open-input-file (string-append path "/" module-name))))

      (when skip-define-module?
        ;; Skip the 'define-module' part.
        (read port))

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

(define (fsm-get-class-code fsm-name)
  (let ((cname (string->symbol (format #f "<~a>" fsm-name))))
    `(define-class ,cname ()
       (debug-mode?
        #:init-value   #f
        #:init-keyword #:debug-mode?
        #:getter       fsm-debug-mode?
        #:setter       fsm-debug-mode-set!))))

(define-method-with-docs (fsm->standalone-code (fsm <fsm>) fsm-name)
  "Convert an @var{fsm} to a procedure that does not depend on Guile-SMC."
  (let ((cname (string->symbol (format #f "<~a>" fsm-name))))
    `(define-method (fsm-run! (fsm ,cname) context)
       ,(if (fsm-description fsm)
            (fsm-description fsm)
            "")
       ,@(fsm-transition-table->standalone-code fsm)
       ,(list (state-name (fsm-current-state fsm)) 'context))))

;;; guile-standalone.scm ends here.

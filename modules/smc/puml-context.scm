;;; puml-context.scm -- PlantUML parser context.

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

;; This file contains a parser context for PlantUML[1] format.
;;
;; 1: http://www.plantuml.com/


;;; Code:

(define-module (smc puml-context)
  #:use-module (oop goops)
  #:use-module (smc core common)
  #:use-module (smc core stack)
  #:use-module (smc core set)
  #:use-module (smc context char-context)
  #:use-module (smc fsm)
  #:export (<puml-context>
            puml-context-fsm
            puml-context-fsm-set!
            puml-context-fsm-event-source
            puml-context-module
            puml-context-keep-going?
            puml-context-resolved-procedures
            puml-context-unresolved-procedures
            %puml-error
            puml-error
            stack-content->string
            context-buffer->string
            resolve-procedure
            guard:title?
            action:no-start-tag-error
            action:unexpected-end-of-file-error))


(define-class <puml-context> (<char-context>)
  ;; The output FSM of the PUML parser.
  ;;
  ;; <fsm>
  (fsm
   #:getter       puml-context-fsm
   #:setter       puml-context-fsm-set!)

  ;; <symbol>
  (fsm-event-source
   #:init-keyword #:fsm-event-source
   #:getter       puml-context-fsm-event-source)

  ;; Modules that contain state machine procedures.
  (module
   #:init-keyword #:module
   #:getter       puml-context-module)

  ;; Whether the parser should keep going when a procedure cannot be resolved
  ;; or not.
  ;;
  ;; When set to #t, the parser remembers all unresolved procedures but keeps
  ;; going without issuing an error.  All unresolved procedures are replaced
  ;; with default variants ('guard:t' for guards, 'action:no-op' for actions.)
  ;;
  ;; <boolean>
  (keep-going?
   #:init-value   #f
   #:init-keyword #:keep-going?
   #:getter       puml-context-keep-going?)

  ;; This set contains resolved procedures.
  ;;
  ;; <set>
  (resolved-procedures
   #:getter       puml-context-resolved-procedures
   #:init-thunk   (lambda () (make <set>)))

  ;; This set contains unresolved procedures.
  ;;
  ;; <set>
  (unresolved-procedures
   #:getter       puml-context-unresolved-procedures
   #:init-thunk   (lambda () (make <set>))))



(define-method (initialize (puml-context <puml-context>) initargs)
  (next-method)
  (let* ((event-source-name (puml-context-fsm-event-source puml-context))
         (event-source (resolve-procedure puml-context event-source-name)))
    (if event-source
        (begin
          (set-add! (puml-context-resolved-procedures puml-context)
                    event-source)
          (context-log-info puml-context
                            "FSM global event source: ~a~%"
                            (cdr event-source))
          (puml-context-fsm-set! puml-context
                                 (make <fsm>
                                   #:event-source (cdr event-source))))
        (if (puml-context-keep-going? puml-context)
            (begin
              (context-log-error puml-context
                                 "Could not resolve procedure ~a in ~a"
                                 event-source-name
                                 puml-context)
              (set-add! (puml-context-unresolved-procedures puml-context)
                        event-source-name)
              (puml-context-fsm-set! puml-context
                                     (make <fsm>
                                       #:event-source (const #t))))
            (puml-error puml-context
                        "Could not resolve procedure ~a in ~a"
                        event-source-name
                        puml-context)))))



;; This procedure tries to resolve a procedure PROC-NAME in the provided
;; modules.
;;
;; Return a pair which 'car' is the module and 'cdr' -- the resolved
;; procedure.  When a procedure cannot be resolved, return #f.
(define (resolve-procedure ctx proc-name)
  (and proc-name
       (let ((modules (puml-context-module ctx)))
         (let loop ((mods modules))
           (if (null? mods)
               (begin
                 (context-log-error ctx
                                    "Could not find \"~a\" procedure in provided modules: ~a"
                                    proc-name
                                    modules)
                 (context-log-error ctx
                                    "Stanza: ~a"
                                    (stanza->list-of-symbols
                                     (context-stanza ctx)))
                 #f)
               (let ((proc (safe-module-ref (car mods) proc-name)))
                 (if proc
                     (cons (car mods) proc)
                     (loop (cdr mods)))))))))


;;; Misc. helper procedures.

(define-method (stack-content->string (stack <stack>))
  (list->string (stack-content/reversed stack)))

;; Convert a context buffer of PUML-CONTEXT to a string.
(define-method (context-buffer->string (puml-context <puml-context>))
  (stack-content->string (context-buffer puml-context)))


;;; Error reporting.

(define %puml-error 'puml-error)

(define puml-error
  (case-lambda
    ((ctx message)
     (context-log-error ctx message)
     (throw %puml-error message))
    ((ctx message . args)
     (apply context-log-error ctx message args)
     (throw %puml-error (apply format #f message args) args))))



(define (guard:title? ctx ch)
  (and (char=? ch #\space)
       (string=? (context-buffer->string ctx) "title")))

(define (action:no-start-tag-error ctx ch)
  (puml-error ctx "No start tag found"))

(define (action:unexpected-end-of-file-error ctx ch)
  (puml-error ctx "Unexpected end of file"))

;;; puml-context.scm ends here.

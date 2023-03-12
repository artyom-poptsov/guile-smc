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
  #:use-module (ice-9 regex)
  #:use-module (smc core common)
  #:use-module (smc core set)
  #:use-module (smc core state)
  #:use-module (smc context char)
  #:use-module (smc context oop char)
  #:use-module (smc fsm)
  #:export (<puml-context>
            puml-context-fsm
            puml-context-fsm-set!
            puml-context-fsm-event-source
            puml-context-module
            puml-context-keep-going?
            puml-context-resolved-procedures
            puml-context-unresolved-procedures
            puml-context-print-resolver-status
            event-source
            %puml-error
            puml-error
            stanza->list-of-symbols
            context-buffer->string
            resolve-procedure
            parse-event-source
            parse-entry-action
            parse-exit-action
            guard:title?

            ;; Actions.
            add-description
            add-state-transition
            process-state-description
            validate-start-tag
            validate-end-tag
            throw-unexpected-end-of-file-error
            throw-no-start-tag-error

            ;; Deprecated.
            action:add-description
            action:add-state-transition
            action:process-state-description
            action:check-start-tag
            action:check-end-tag
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
   #:init-thunk   (lambda () (make-hash-table)))

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



(define event-source char-context-event-source)



;; This procedure tries to resolve a procedure PROC-NAME in the provided
;; modules.
;;
;; Return a pair which 'car' is the module and 'cdr' -- the resolved
;; procedure.  When a procedure cannot be resolved, return #f.
(define (resolve-procedure ctx proc-name)
  (and proc-name
       (let* ((resolved-procs (puml-context-resolved-procedures ctx))
              (p              (hash-ref resolved-procs proc-name)))
         (if p
             p
             (let* ((modules (puml-context-module ctx))
                    (result  (safe-module-list-ref modules proc-name)))
               (if result
                   (begin
                     (hash-set! resolved-procs proc-name result)
                     result)
                   (begin
                     (context-log-error ctx
                                        "Could not find \"~a\" procedure in provided modules: ~a"
                                        proc-name
                                        modules)
                     (context-log-error ctx
                                        "Stanza: ~a"
                                        (stanza->list-of-symbols
                                         (context-stanza ctx)))
                     (set-add! (puml-context-unresolved-procedures ctx) proc-name)
                     #f)))))))

(define (module-name module)
  (let ((str (object->string module)))
    (substring/read-only str
                         (string-index str #\()
                         (+ (string-index str #\)) 1))))

(define-method (puml-context-print-resolver-status (puml-context <puml-context>)
                                                   (port         <port>))
  (display ";;; Resolver status:\n" port)
  (let loop ((procedures (sort (hash-map->list cons
                                               (puml-context-resolved-procedures
                                                puml-context))
                               (lambda (y x)
                                 (string<? (object->string (cdr y))
                                           (object->string (cdr x))))))
             (current-module #f))
    (unless (null? procedures)
      (let* ((entry (cdar procedures))
             (module (car entry))
             (proc   (cdr entry)))
        (unless (equal? current-module module)
          (format port ";;;   #<directory ~a>~%" (module-name module)))
        (format port ";;;     ~a~%" proc)
        (loop (cdr procedures) module))))

  (unless (set-empty? (puml-context-unresolved-procedures puml-context))
    (display ";;;\n;;;   Unresolved procedures:\n" port)
    (for-each (lambda (proc-name)
                (format port ";;;     ~a~%" proc-name))
              (set-content (puml-context-unresolved-procedures puml-context)))))


;;; Misc. helper procedures.

(define-method (stanza->list-of-symbols (stanza <list>))
  (map (lambda (elem)
         (string->symbol (list->string elem)))
       (reverse stanza)))

;; Convert a context buffer of PUML-CONTEXT to a string.
(define-method (context-buffer->string (puml-context <puml-context>))
  (list->string (reverse (context-buffer puml-context))))


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


(define (action:add-description ctx ch)
  (fsm-description-set! (puml-context-fsm ctx) (context-buffer->string ctx))
  (context-clear! ctx)
  ctx)

(define (action:check-start-tag ctx ch)
  (let ((tag (context-buffer->string ctx)))
    (unless (string=? tag "@startuml")
      (puml-error ctx "Misspelled @startuml"))
    (context-buffer-clear! ctx)
    ctx))

(define (action:check-end-tag ctx)
  (let* ((tag (context-buffer->string ctx)))
    (unless (string=? tag "@enduml")
      (puml-error ctx "Misspelled @enduml"))
    (context-buffer-clear! ctx)
    ctx))

(define add-description action:add-description)
(define validate-start-tag action:check-start-tag)
(define validate-end-tag action:check-end-tag)
(define throw-unexpected-end-of-file-error action:unexpected-end-of-file-error)
(define throw-no-start-tag-error action:no-start-tag-error)



(define (%puml-transition:from tr)
  (list-ref tr 0))

(define (%puml-transition:to tr)
  (list-ref tr 1))

(define (%puml-transition:tguard tr)
  (and (> (length tr) 2)
       (list-ref tr 2)))

(define (%puml-transition:action tr)
  (and (= (length tr) 4)
       (list-ref tr 3)))



(define (%context-fsm-state-add! ctx state-name)
  (let ((state (make <state>
                 #:name state-name
                 #:event-source (fsm-event-source (puml-context-fsm ctx)))))
    (fsm-state-add! (puml-context-fsm ctx) state)))



(define (action:add-state-transition ctx ch)

  (unless (null? (context-buffer ctx))
    (action:update-stanza ctx ch))

  (let* ((fsm     (puml-context-fsm ctx))
         (stanza  (stanza->list-of-symbols (context-stanza ctx)))
         (module  (puml-context-module ctx))
         (from    (%puml-transition:from   stanza))
         (to      (%puml-transition:to     stanza))
         (tguard  (%puml-transition:tguard stanza))
         (action  (%puml-transition:action stanza)))
    (context-log-debug ctx
                       "action:add-state-transition: [~a] -> [~a]: ~a -> ~a"
                       from to
                       tguard action)
    (cond
     ((equal? from '*)
      (context-log-debug ctx
                         "action:add-state-transition: Adding first state...")
      (%context-fsm-state-add! ctx to)
      (fsm-current-state-set! fsm (fsm-state fsm to)))
     ((and (equal? from '*) (equal? to '*))
      (puml-error ctx "Meaningless transition: [*] -> [*]"))
     (else
      (let ((resolved-tguard (if tguard
                                 (resolve-procedure ctx tguard)
                                 (resolve-procedure ctx 'guard:#t)))
            (resolved-action (if action
                                 (resolve-procedure ctx action)
                                 (resolve-procedure ctx 'action:no-op))))

        (unless resolved-tguard
          (if (puml-context-keep-going? ctx)
              (context-log-error ctx "Could not resolve procedure ~a in ~a" tguard ctx)
              (puml-error ctx "Could not resolve procedure ~a in ~a" tguard ctx)))

        (unless resolved-action
          (if (puml-context-keep-going? ctx)
              (context-log-error ctx "Could not resolve procedure ~a in ~a" action ctx)
              (puml-error ctx "Could not resolve procedure ~a in ~a" action ctx)))

        (unless (fsm-state fsm from)
          (%context-fsm-state-add! ctx from))

        (unless (or (equal? to '*) (fsm-state fsm to))
          (%context-fsm-state-add! ctx to))

        (fsm-transition-add! fsm from
                             (if resolved-tguard
                                 (cdr resolved-tguard)
                                 guard:#t)
                             (if resolved-action
                                 (cdr resolved-action)
                                 action:no-op)
                             (if (equal? to '*)
                                 #f
                                 to)))))

    (context-stanza-clear! ctx)

    ctx))

(define add-state-transition action:add-state-transition)


;; Try to parse a LINE as event source definition.  Returns a match or #f if
;; line does not match.
;;
;; Example event source definition:
;;
;;   event-source: some-event-source
;;
(define-method (parse-event-source (line <string>))
  (string-match "[ \t]+event-source:[ \t]+([^ \t\n]+)" line))

;; Try to parse a LINE as an entry action definition.  Returns a match or #f if
;; line does not match.
;;
;; Example entry action definition:
;;
;;   entry-action: some-entry-action
;;
(define-method (parse-entry-action (line <string>))
  (string-match "[ \t]+entry-action:[ \t]+([^ \t\n]+)" line))

;; Try to parse a LINE as an exit action definition.  Returns a match or #f if
;; line does not match.
;;
;; Example entry action definition:
;;
;;   exit-action: some-exit-action
;;
(define-method (parse-exit-action (line <string>))
  (string-match "[ \t]+exit-action:[ \t]+([^ \t\n]+)" line))

(define (action:process-state-description ctx ch)

  (define (%resolve state-name proc-name)
    (let* ((proc-name (string->symbol proc-name))
           (proc      (resolve-procedure ctx proc-name)))
      (if proc
          (begin
            (context-log-info ctx "[~a] Resolved \"~a\" from \"~a\""
                              state-name (cdr proc) (car proc))
            (cdr proc))
          (if (puml-context-keep-going? ctx)
              (begin
                (context-log-error ctx "[~a] Could not resolve procedure ~a in ~a"
                                   state-name proc-name ctx)
                (set-add! (puml-context-unresolved-procedures ctx) proc-name)
                #f)
              (puml-error ctx "[~a] Cannot resolve event source \"~a\""
                          state-name
                          proc-name)))))

  (let* ((fsm             (puml-context-fsm ctx))
         (stanza          (stanza->list-of-symbols (context-stanza ctx)))
         (state-name      (list-ref stanza 0))
         (description     (and (fsm-state fsm state-name)
                               (state-description
                                (fsm-state fsm state-name))))
         (new-description (context-buffer->string ctx)))

    (when (equal? state-name (string->symbol "*"))
      (puml-error ctx "[*] cannot have description"))

    (unless (fsm-state fsm state-name)
      (%context-fsm-state-add! ctx state-name))

    (let ((event-source-match (parse-event-source new-description))
          (entry-action-match (parse-entry-action new-description))
          (exit-action-match  (parse-exit-action new-description))
          (state              (fsm-state fsm state-name)))
      (cond
       (event-source-match
        (let ((event-source (%resolve state-name
                                      (match:substring event-source-match 1))))
          (state-event-source-set! state event-source)))
       (entry-action-match
        (let ((entry-action (%resolve state-name
                                      (match:substring entry-action-match 1))))
          (state-entry-action-set! state entry-action)))
       (exit-action-match
        (let ((exit-action (%resolve state-name
                                     (match:substring exit-action-match 1))))
          (state-exit-action-set! state exit-action)))
       (description
        (fsm-state-description-add! fsm
                                    state-name
                                    (string-append
                                     description
                                     " "
                                     new-description)))
       (else
        (fsm-state-description-add! fsm
                                    state-name
                                    new-description))))
    (context-clear! ctx)
    ctx))

(define process-state-description action:process-state-description)

;;; puml-context.scm ends here.

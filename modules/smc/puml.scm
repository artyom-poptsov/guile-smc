;;; puml.scm -- PlantUML parser written in Guile-SMC.

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

;; This file contains a parser implementation for PlantUML[1] format.
;;
;; 1: http://www.plantuml.com/


;;; Code:

(define-module (smc puml)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (smc core state)
  #:use-module (smc fsm)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:use-module (smc core stack)
  #:use-module (smc core set)
  #:use-module (smc context char-context)
  #:export (<puml-context>
            resolve-procedure
            puml-context-fsm
            puml-context-module
            puml-context-keep-going?
            puml-context-resolved-procedures
            puml-context-unresolved-procedures
            puml-context-print-resolver-status
            puml->fsm
            puml-string->fsm))



(define-class <puml-context> (<char-context>)
  ;; The output FSM of the PUML parser.
  ;;
  ;; <fsm>
  (fsm
   #:getter       puml-context-fsm
   #:setter       puml-context-fsm-set!)

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
  (let ((event-source (resolve-global-event-source puml-context)))
    (context-log-info puml-context "FSM global event source: ~a~%"
                      event-source)
    (puml-context-fsm-set! puml-context
                           (make <fsm>
                             #:event-source event-source))))


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


;;; Misc. helper procedures.

(define-method (stack-content->string (stack <stack>))
  (list->string (stack-content/reversed stack)))



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

(define-method (stanza->list-of-symbols (stanza <stack>))
  (map (lambda (elem)
         (string->symbol (list->string elem)))
       (stack-content/reversed stanza)))


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


(define %event-source-prefix "event-source")

(define (resolve-global-event-source ctx)
  (let* ((proc (resolve-procedure ctx (string->symbol %event-source-prefix))))
    (if proc
        (begin
          (set-add! (puml-context-resolved-procedures ctx)
                    proc)
          (cdr proc))
        (const #t))))


(define (%context-fsm-state-add! ctx state-name)
  (let ((state (make <state>
                 #:name         state-name
                 #:event-source (resolve-global-event-source ctx))))
    (fsm-state-add! (puml-context-fsm ctx) state)))


(define (action:add-state-transition ctx ch)

  (unless (stack-empty? (context-buffer ctx))
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

        (if resolved-tguard
            (set-add! (puml-context-resolved-procedures ctx) resolved-tguard)
            (if (puml-context-keep-going? ctx)
                (begin
                  (context-log-error ctx "Could not resolve procedure ~a in ~a" tguard ctx)
                  (set-add! (puml-context-unresolved-procedures ctx) tguard))
                (puml-error ctx "Could not resolve procedure ~a in ~a" tguard ctx)))

        (if resolved-action
            (set-add! (puml-context-resolved-procedures ctx) resolved-action)
            (if (puml-context-keep-going? ctx)
                (begin
                  (context-log-error ctx "Could not resolve procedure ~a in ~a" action ctx)
                  (set-add! (puml-context-unresolved-procedures ctx) action))
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


;; Try to parse a LINE as event source definition.  Returns a match or #f if
;; line does not match.
;;
;; Example event source definition:
;;
;;   event-source: some-event-source
;;
(define-method (parse-event-source (line <string>))
  (string-match "[ \t]+event-source:[ \t]+([^ \t\n]+)" line))

(define (action:process-state-description ctx ch)
  (let* ((fsm             (puml-context-fsm ctx))
         (stanza          (stanza->list-of-symbols (context-stanza ctx)))
         (buf             (context-buffer ctx))
         (state-name      (list-ref stanza 0))
         (description     (and (fsm-state fsm state-name)
                               (state-description
                                (fsm-state fsm state-name))))
         (new-description (stack-content->string buf)))

    (when (equal? state-name (string->symbol "*"))
      (puml-error ctx "[*] cannot have description"))

    (unless (fsm-state fsm state-name)
      (%context-fsm-state-add! ctx state-name))

    (let ((event-source-match (parse-event-source new-description)))
      (cond
       (event-source-match
        (let* ((state             (fsm-state fsm state-name))
               (event-source-name (string->symbol
                                   (match:substring event-source-match 1)))
               (event-source      (resolve-procedure ctx event-source-name)))
          (unless event-source
            (if (puml-context-keep-going? ctx)
                (begin
                  (context-log-error ctx "Could not resolve procedure ~a in ~a"
                                     event-source-name ctx)
                  (set-add! (puml-context-unresolved-procedures ctx)
                            event-source-name))
                (puml-error ctx "[~a] Cannot resolve event source \"~a\""
                            state-name
                            event-source-name)))
          (context-log-info ctx "[~a] Resolved event source \"~a\" from \"~a\""
                            state-name (cdr event-source) (car event-source))
          (set-add! (puml-context-resolved-procedures ctx)
                    event-source)
          (state-event-source-set! state (cdr event-source))))
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
    (context-buffer-clear! ctx)
    (context-stanza-clear! ctx)
    ctx))

(define (action:add-description ctx ch)
  (let ((fsm (puml-context-fsm ctx))
        (buf (context-buffer ctx)))
    (fsm-description-set! fsm (stack-content->string buf))
    (context-buffer-clear! ctx)
    (context-stanza-clear! ctx)
    ctx))


(define (action:check-start-tag ctx ch)
  (let* ((buf (context-buffer ctx))
         (str (stack-content->string buf)))
    (unless (string=? str "@startuml")
      (puml-error ctx "Misspelled @startuml"))
    (context-buffer-clear! ctx)
    ctx))

(define (action:check-end-tag ctx ch)
  (let* ((buf (context-buffer ctx))
         (str (stack-content->string buf)))
    (unless (string=? str "@enduml")
      (puml-error ctx "Misspelled @enduml"))
    (context-buffer-clear! ctx)
    ctx))

(define (action:no-start-tag-error ctx ch)
  (puml-error ctx "No start tag found"))

(define (action:unexpected-end-of-file-error ctx ch)
  (puml-error ctx "Unexpected end of file"))


(define (guard:title? ctx ch)
  (let ((buf (context-buffer ctx)))
    (and (char=? ch #\space)
         (string=? (stack-content->string buf)
                   "title"))))



(define %transition-table
  `(((name        . read-start-tag)
     (description . "Read the start @startuml tag and check it for errors.")
     (transitions
      (,guard:eof-object?     ,action:unexpected-end-of-file-error #f)
      (,guard:space?          ,action:check-start-tag    read)
      (,guard:newline?        ,action:check-start-tag    read)
      (,guard:#t              ,action:store       read-start-tag)))
    ((name        . read)
     (description . "Read the PlantUML transition table.")
     (transitions
      (,guard:eof-object?          ,action:unexpected-end-of-file-error #f)
      (,guard:at-symbol?           ,action:store        read-end-tag)
      (,guard:single-quote?        ,action:no-op        read/skip-comment)
      (,guard:left-square-bracket? ,action:no-op        read-state)
      (,guard:letter?              ,action:store        read-word)
      (,guard:#t                   ,action:no-op        read)))
    ((name         . read-end-tag)
     (description  . "Read the @enduml tag.")
     (transitions
      (,guard:eof-object?     ,action:check-end-tag     #f)
      (,guard:newline?        ,action:check-end-tag     #f)
      (,guard:space?          ,action:check-end-tag     #f)
      (,guard:#t              ,action:store             read-end-tag)))
    ((name         . read/skip-comment)
     (description  . "Skip commentaries that are written between stanzas.")
     (transitions
      (,guard:eof-object?     ,action:no-op             #f)
      (,guard:newline?        ,action:no-op             read)
      (,guard:#t              ,action:no-op             read/skip-comment)))
    ((name         . read-word)
     (description  . "Read a word.")
     (transitions
      (,guard:eof-object?    ,action:no-op              #f)
      (,guard:title?         ,action:clear-buffer       read-title)
      (,guard:colon?         ,action:update-stanza      read-state-description)
      (,guard:space?         ,action:update-stanza      search-state-transition)
      (,guard:#t             ,action:store              read-word)))
    ((name         . read-title)
     (description  . "Read a diagram title.")
     (transitions
      (,guard:eof-object?    ,action:no-op              #f)
      (,guard:newline?       ,action:add-description    read)
      (,guard:#t             ,action:store              read-title)))
    ((name         . read-state)
     (description  . "Read a PlantUML stanza.")
     (transitions
      (,guard:eof-object?           ,action:no-op                #f)
      (,guard:newline?              ,action:syntax-error         #f)
      (,guard:right-square-bracket? ,action:update-stanza search-state-transition)
      (,guard:space?                ,action:update-stanza search-state-transition)
      (,guard:colon?                ,action:update-stanza read-state-description)
      (,guard:#t                    ,action:store         read-state)))
    ((name         . search-state-transition)
     (description  . "Check if a state has a transition.")
     (transitions
      (,guard:eof-object?     ,action:no-op        #f)
      (,guard:colon?          ,action:no-op        read-state-description)
      (,guard:hyphen-minus?           ,action:no-op        read-state-right-arrow)
      (,guard:less-than-sign? ,action:no-op        read-state-left-arrow)
      (,guard:#t              ,action:no-op        search-state-transition)))
    ((name         . read-state-description)
     (description  . "Read a state description if it is present.")
     (transitions
      (,guard:eof-object?     ,action:no-op                 #f)
      (,guard:newline?        ,action:process-state-description read)
      (,guard:#t              ,action:store          read-state-description)))
    ((name         . read-state-right-arrow)
     (description  . "Read a right arrow that indicates a transition.")
     (transitions
      (,guard:eof-object?     ,action:no-op        #f)
      (,guard:space?          ,action:no-op        search-state-transition-to)
      (,guard:#t              ,action:no-op        read-state-right-arrow)))
    ((name         . search-state-transition-to)
     (description  . "Search for a state that the current state transitions to.")
     (transitions
      (,guard:eof-object?          ,action:no-op        #f)
      (,guard:letter?              ,action:store read-state-transition-to)
      (,guard:left-square-bracket? ,action:no-op        read-state-transition-to)
      (,guard:#t                   ,action:no-op        search-state-transition-to)))
    ((name         . read-state-transition-to)
     (description  . "Read a state that the current state transitions to.")
     (transitions
      (,guard:eof-object?           ,action:no-op                #f)
      ;; (,guard:space?          ,action:no-op                        read-state-transition-guard)
      (,guard:right-square-bracket? ,action:no-op                read-state-transition-to)
      (,guard:colon?                ,action:update-stanza search-state-transition-guard)
      (,guard:newline?              ,action:add-state-transition read)
      (,guard:#t                    ,action:store         read-state-transition-to)))
    ((name         . search-state-transition-guard)
     (description  . "Check if the transition has a guard.")
     (transitions
      (,guard:eof-object?     ,action:no-op                      #f)
      (,guard:letter?         ,action:store               read-state-transition-guard)
      (,guard:#t              ,action:no-op                      search-state-transition-guard)))
    ((name         . read-state-transition-guard)
     (description  . "Read a state transition guard.")
     (transitions
      (,guard:eof-object?     ,action:no-op                      #f)
      (,guard:space?          ,action:update-stanza       search-state-action-arrow)
      (,guard:newline?        ,action:add-state-transition       read)
      (,guard:#t              ,action:store               read-state-transition-guard)))
    ((name         . search-state-action-arrow)
     (description  . "Check if a transition has an attached action.")
     (transitions
      (,guard:eof-object?     ,action:no-op                      #f)
      (,guard:newline?        ,action:no-op                      read)
      (,guard:hyphen-minus?           ,action:no-op                      read-state-action-arrow)
      (,guard:#t              ,action:no-op                      search-state-action-arrow)))
    ((name         . read-state-action-arrow)
     (description  . "Read and skip the action arrow.")
     (transitions
      (,guard:eof-object?      ,action:unexpected-end-of-file-error #f)
      (,guard:newline?         ,action:no-op                        #f)
      (,guard:more-than-sign? ,action:no-op                        search-state-transition-action)))
    ((name         . search-state-transition-action)
     (description  . "Check if an action is present after the arrow.  Issue an error if it is not.")
     (transitions
      (,guard:eof-object?      ,action:unexpected-end-of-file-error #f)
      (,guard:letter?          ,action:store                 read-state-transition-action)
      (,guard:newline?         ,action:no-op                        #f)
      (,guard:#t               ,action:no-op                        search-state-transition-action)))
    ((name         . read-state-transition-action)
     (description  . "Read the state transition action.")
     (transitions
      (,guard:eof-object?      ,action:unexpected-end-of-file-error #f)
      (,guard:newline?         ,action:add-state-transition         read)
      (,guard:#t               ,action:store                 read-state-transition-action)))))



(define-method (puml-context-print-resolver-status (puml-context <puml-context>)
                                                   (port         <port>))
  (display ";;; Resolver status:\n" port)
  (let loop ((procedures (sort (set-content (puml-context-resolved-procedures
                                             puml-context))
                               (lambda (y x)
                                 (string<? (object->string y) (object->string x)))))
             (current-module #f))
    (unless (null? procedures)
      (let* ((entry (car procedures))
             (module (car entry))
             (proc   (cdr entry)))
        (unless (equal? current-module module)
          (format port ";;;   ~a~%" module))
        (format port ";;;     ~a~%" proc)
        (loop (cdr procedures) module))))

  (unless (set-empty? (puml-context-unresolved-procedures puml-context))
    (display ";;;\n;;;   Unresolved procedures:\n" port)
    (for-each (lambda (proc-name)
                (format port ";;;     ~a~%" proc-name))
              (set-content (puml-context-unresolved-procedures puml-context)))))



;; Read state machine description in PlantUML format from a PORT and parse it.
;;
;; Required parameters:
;;   port               A port from which PlantUML data is read.
;;
;; Keyword parameters:
;;   module             A list of modules that needed to resolve output FSM
;;                      procedures.
;;                      Default value: (current-module)
;;   keep-going?        If this parameter is set to #t, the parser ignores
;;                      unresolved procedure errors.  All unresolved procedures
;;                      are replaced with default values.
;;                      Beware that the output FSM in this case may be invalid.
;;                      Default value: #f
;;   debug-mode?        If set to #t, the debug mode is enabled.
;;                      Default value: #f
;;
;; Return a finite-state machine produced from the input data.
(define* (puml->fsm port
                    #:key
                    (module (current-module))
                    (keep-going? #f)
                    (debug-mode? #f))
  (log-use-stderr! debug-mode?)
  (let* ((reader-fsm
          (make <fsm>
            #:description (string-append
                           "PlantUML <http://www.plantuml.com> Reader Finite-State Machine.\n"
                           "This FSM is a part of Guile State-Machine Compiler (Guile-SMC)\n"
                           "<https://github.com/artyom-poptsov/guile-smc>")
            #:debug-mode? debug-mode?
            #:event-source     event-source
            #:transition-table %transition-table))
         (context (fsm-run! reader-fsm
                            (make <puml-context>
                              #:port        port
                              #:module      module
                              #:keep-going? keep-going?)))
         (output-fsm (puml-context-fsm context)))
      (fsm-parent-set! output-fsm reader-fsm)
      (fsm-parent-context-set! output-fsm context)
      (when debug-mode?
        (fsm-pretty-print-statistics reader-fsm (current-error-port)))
      output-fsm))

(define* (puml-string->fsm string
                           #:key
                           (module (list (current-module)))
                           (keep-going? #f)
                           (debug-mode? #f))
  (with-input-from-string
      string
      (lambda ()
        (puml->fsm (current-input-port)
                   #:module      module
                   #:keep-going? keep-going?
                   #:debug-mode? debug-mode?))))

;;; puml.scm ends here.

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
  #:use-module (ice-9 textual-ports)
  #:use-module ((ice-9 format)
                #:prefix ice-9:)
  #:use-module (smc core state)
  #:use-module (smc fsm)
  #:use-module (smc core log)
  #:use-module (smc core stack)
  #:use-module (smc context char-context)
  #:export (<puml-context>
            resolve-procedure
            puml-context-fsm
            puml-context-module
            puml->fsm
            puml-string->fsm))



(define-class <puml-context> (<char-context>)
  (fsm
   #:init-value (make <fsm>)
   #:getter     puml-context-fsm)

  ;; A module which contains state machine procedures.
  (module
   #:init-keyword #:module
   #:getter     puml-context-module))



;; This procedure tries to resolve a procedure PROC-NAME in the provided
;; modules. When no procedure available with the given name, returns DEFAULT
;; procedure.
(define (resolve-procedure ctx proc-name default)
  (let ((module (puml-context-module ctx)))
    (cond
     ((not proc-name)
      default)
     ((list? module)
      (let loop ((mods module))
        (let ((proc (catch
                      #t
                      (lambda ()
                        (module-ref (car mods) proc-name))
                      (const #f))))
          (cond
           (proc
            proc)
           ((and (not proc) (null? mods))
            (log-error
             "input:~a:~a: Could not find \"~a\" procedure in provided modules: ~a"
             (char-context-row ctx)
             (char-context-col ctx)
             proc-name
             (puml-context-module ctx))
            (log-error
             "input:~a:~a: Stanza: ~a"
             (char-context-row ctx)
             (char-context-col ctx)
             (stanza->list-of-symbols (context-stanza ctx)))
            (error "Could not find procedure in provided modules"
                   (char-context-row ctx)
                   (char-context-col ctx)
                   proc-name))
           (else
            (loop (cdr mods)))))))
     (else
      (module-ref module proc-name)))))

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


(define (action:add-state-transition ctx ch)

  (unless (stack-empty? (context-buffer ctx))
    (action:update-stanza ctx ch))

  (let* ((fsm     (puml-context-fsm ctx))
         (stanza  (stanza->list-of-symbols (context-stanza ctx)))
         (module  (puml-context-module ctx))
         (from    (puml-transition:from   stanza))
         (to      (puml-transition:to     stanza))
         (tguard  (puml-transition:tguard stanza))
         (action  (puml-transition:action stanza)))
    (log-debug "input:~a:~a: action:add-state-transition: [~a] -> [~a]: ~a -> ~a"
               (char-context-row ctx)
               (char-context-col ctx)
               from to
               tguard action)
    (cond
     ((equal? from '*)
      (log-debug "input:~a:~a: action:add-state-transition: Adding first state..."
                 (char-context-row ctx)
                 (char-context-col ctx))
      (let ((state (make <state> #:name to)))
        (fsm-state-add! fsm state)
        (fsm-current-state-set! fsm state)))
     ((and (equal? from '*) (equal? to '*))
      (log-error
       "input~a:~a: Meaningless transition: [*] -> [*]"
       (char-context-row ctx)
       (char-context-col ctx))
      (error "Meaningless transition: [*] -> [*]"))
     (else
      (fsm-transition-add! fsm
                           from
                           (resolve-procedure ctx tguard guard:#t)
                           (resolve-procedure ctx action action:no-op)
                           (if (equal? to '*)
                               #f
                               to))))

    (context-stanza-clear! ctx)

    ctx))

(define (action:add-state-description ctx ch)
  (let* ((fsm             (puml-context-fsm ctx))
         (stanza          (stanza->list-of-symbols (context-stanza ctx)))
         (buf             (context-buffer ctx))
         (state-name      (list-ref stanza 0))
         (description     (and (fsm-state fsm state-name)
                               (state-description
                                (fsm-state fsm state-name))))
         (new-description (list->string (stack-content/reversed buf))))

    (when (equal? state-name (string->symbol "*"))
      (log-error
       "input:~a:~a: [*] cannot have description"
       (char-context-row ctx)
       (char-context-col ctx))
      (error "[*] cannot have description"))

    (if description
        (fsm-state-description-add! fsm
                                    state-name
                                    (string-append
                                     description
                                     " "
                                     new-description))
        (fsm-state-description-add! fsm
                                    state-name
                                    new-description))
    (context-buffer-clear! ctx)
    (context-stanza-clear! ctx)
    ctx))

(define (action:add-description ctx ch)
  (let ((fsm (puml-context-fsm ctx))
        (buf (context-buffer ctx)))
    (fsm-description-set! fsm (list->string (stack-content/reversed buf)))
    (context-buffer-clear! ctx)
    (context-stanza-clear! ctx)
    ctx))


(define (action:check-start-tag ctx ch)
  (let* ((buf (context-buffer ctx))
         (str (list->string (stack-content/reversed buf))))
    (unless (string=? str "@startuml")
      (log-error
       "input:~a:~a: Misspelled @startuml"
       (char-context-row ctx)
       (char-context-col ctx))
      (error "Misspelled @startuml" str))
    (context-buffer-clear! ctx)
    ctx))

(define (action:check-end-tag ctx ch)
  (let* ((buf (context-buffer ctx))
         (str (list->string (stack-content/reversed buf))))
    (unless (string=? str "@enduml")
      (log-error
       "input:~a:~a: Misspelled @enduml"
       (char-context-row ctx)
       (char-context-col ctx))
      (error "Misspelled @enduml" str))
    (context-buffer-clear! ctx)
    ctx))

(define (action:no-start-tag-error ctx ch)
  (log-error
   "input:~a:~a: No start tag found"
   (char-context-row ctx)
   (char-context-col ctx))
  (error "No start tag found"))

(define (action:unexpected-end-of-file-error ctx ch)
  (log-error
   "input:~a:~a: Unexpected end of file"
   (char-context-row ctx)
   (char-context-col ctx))
  (error "Unexpected end of file"))


(define (guard:title? ctx ch)
  (let ((buf (context-buffer ctx)))
    (and (char=? ch #\space)
         (string=? (list->string (stack-content/reversed buf))
                   "title"))))



(define %transition-table
  `((read-start-tag
     "Read the start @startuml tag and check it for errors."
     (,guard:eof-object?     ,action:unexpected-end-of-file-error #f)
     (,guard:space?          ,action:check-start-tag    read)
     (,guard:newline?        ,action:check-start-tag    read)
     (,guard:#t              ,action:store       read-start-tag))
    (read
     "Read the PlantUML transition table."
     (,guard:eof-object?          ,action:unexpected-end-of-file-error #f)
     (,guard:at-symbol?           ,action:store        read-end-tag)
     (,guard:single-quote?        ,action:no-op        read/skip-comment)
     (,guard:left-square-bracket? ,action:no-op        read-state)
     (,guard:letter?              ,action:store        read-word)
     (,guard:#t                   ,action:no-op        read))
    (read-end-tag
     "Read the @enduml tag."
     (,guard:eof-object?     ,action:check-end-tag     #f)
     (,guard:newline?        ,action:check-end-tag     #f)
     (,guard:space?          ,action:check-end-tag     #f)
     (,guard:#t              ,action:store             read-end-tag))
    (read/skip-comment
     "Skip commentaries that are written between stanzas."
     (,guard:eof-object?     ,action:no-op             #f)
     (,guard:newline?        ,action:no-op             read)
     (,guard:#t              ,action:no-op             read/skip-comment))
    (read-word
     "Read a word."
     (,guard:eof-object?    ,action:no-op              #f)
     (,guard:title?         ,action:clear-buffer       read-title)
     (,guard:colon?         ,action:update-stanza      read-state-description)
     (,guard:space?         ,action:update-stanza      search-state-transition)
     (,guard:#t             ,action:store              read-word))
    (read-title
     "Read a diagram title."
     (,guard:eof-object?    ,action:no-op              #f)
     (,guard:newline?       ,action:add-description    read)
     (,guard:#t             ,action:store              read-title))
    (read-state
     "Read a PlantUML stanza."
     (,guard:eof-object?           ,action:no-op                #f)
     (,guard:newline?              ,action:syntax-error         #f)
     (,guard:right-square-bracket? ,action:update-stanza search-state-transition)
     (,guard:space?                ,action:update-stanza search-state-transition)
     (,guard:colon?                ,action:update-stanza read-state-description)
     (,guard:#t                    ,action:store         read-state))
    (search-state-transition
     "Check if a state has a transition."
     (,guard:eof-object?     ,action:no-op        #f)
     (,guard:colon?          ,action:no-op        read-state-description)
     (,guard:hyphen-minus?           ,action:no-op        read-state-right-arrow)
     (,guard:less-than-sign? ,action:no-op        read-state-left-arrow)
     (,guard:#t              ,action:no-op        search-state-transition))
    (read-state-description
     "Read a state description if it is present."
     (,guard:eof-object?     ,action:no-op                 #f)
     (,guard:newline?        ,action:add-state-description read)
     (,guard:#t              ,action:store          read-state-description))
    (read-state-right-arrow
     "Read a right arrow that indicates a transition."
     (,guard:eof-object?     ,action:no-op        #f)
     (,guard:space?          ,action:no-op        search-state-transition-to)
     (,guard:#t              ,action:no-op        read-state-right-arrow))
    (search-state-transition-to
     "Search for a state that the current state transitions to."
     (,guard:eof-object?          ,action:no-op        #f)
     (,guard:letter?              ,action:store read-state-transition-to)
     (,guard:left-square-bracket? ,action:no-op        read-state-transition-to)
     (,guard:#t                   ,action:no-op        search-state-transition-to))
    (read-state-transition-to
     "Read a state that the current state transitions to."
     (,guard:eof-object?           ,action:no-op                #f)
     ;; (,guard:space?          ,action:no-op                        read-state-transition-guard)
     (,guard:right-square-bracket? ,action:no-op                read-state-transition-to)
     (,guard:colon?                ,action:update-stanza search-state-transition-guard)
     (,guard:newline?              ,action:add-state-transition read)
     (,guard:#t                    ,action:store         read-state-transition-to))
    (search-state-transition-guard
     "Check if the transition has a guard."
     (,guard:eof-object?     ,action:no-op                      #f)
     (,guard:letter?         ,action:store               read-state-transition-guard)
     (,guard:#t              ,action:no-op                      search-state-transition-guard))
    (read-state-transition-guard
     "Read a state transition guard."
     (,guard:eof-object?     ,action:no-op                      #f)
     (,guard:space?          ,action:update-stanza       search-state-action-arrow)
     (,guard:newline?        ,action:add-state-transition       read)
     (,guard:#t              ,action:store               read-state-transition-guard))
    (search-state-action-arrow
     "Check if a transition has an attached action."
     (,guard:eof-object?     ,action:no-op                      #f)
     (,guard:newline?        ,action:no-op                      read)
     (,guard:hyphen-minus?           ,action:no-op                      read-state-action-arrow)
     (,guard:#t              ,action:no-op                      search-state-action-arrow))
    (read-state-action-arrow
     "Read and skip the action arrow."
     (,guard:eof-object?      ,action:unexpected-end-of-file-error #f)
     (,guard:newline?         ,action:no-op                        #f)
     (,guard:more-than-sign? ,action:no-op                        search-state-transition-action))
    (search-state-transition-action
     "Check if an action is present after the arrow.  Issue an error if it is not."
     (,guard:eof-object?      ,action:unexpected-end-of-file-error #f)
     (,guard:letter?          ,action:store                 read-state-transition-action)
     (,guard:newline?         ,action:no-op                        #f)
     (,guard:#t               ,action:no-op                        search-state-transition-action))
    (read-state-transition-action
     "Read the state transition action."
     (,guard:eof-object?      ,action:unexpected-end-of-file-error #f)
     (,guard:newline?         ,action:add-state-transition         read)
     (,guard:#t               ,action:store                 read-state-transition-action))))



(define (fsm-pretty-print-statistics fsm port)
  (display ";;; Statistics:\n" port)
  (for-each (lambda (record)
              (ice-9:format port ";;;   ~10a: ~10a~%"
                            (car record)
                            (cdr record)))
            (fsm-statistics fsm)))

(define* (puml->fsm port
                    #:key
                    (module (current-module))
                    (debug-mode? #f))
  (log-use-stderr! debug-mode?)
  (let ((reader-fsm
         (make <fsm>
           #:description (string-append
                          "PlantUML <http://www.plantuml.com> Reader Finite-State Machine.\n"
                          "This FSM is a part of Guile State-Machine Compiler (Guile-SMC)\n"
                          "<https://github.com/artyom-poptsov/guile-smc>")
           #:debug-mode? debug-mode?
           #:transition-table %transition-table)))

    (let loop ((context (make <puml-context> #:module module)))
      (let ((ch (get-char port)))
        (char-context-update-counters! context ch)
        (receive (new-state new-context)
            (fsm-run! reader-fsm ch context)
          (if new-state
              (loop new-context)
              (let ((output-fsm (puml-context-fsm context)))
                (when debug-mode?
                  (fsm-pretty-print-statistics reader-fsm (current-error-port)))
                output-fsm)))))))

(define* (puml-string->fsm string
                           #:key
                           (module (current-module))
                           (debug-mode? #f))
  (with-input-from-string
      string
      (lambda ()
        (puml->fsm (current-input-port)
                   #:module      module
                   #:debug-mode? debug-mode?))))

;;; puml.scm ends here.

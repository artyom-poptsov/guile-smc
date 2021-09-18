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
  #:use-module (smc core state)
  #:use-module (smc fsm)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:use-module (smc core stack)
  #:use-module (smc core set)
  #:use-module (smc context char-context)
  #:use-module (smc puml-context)
  #:re-export (puml-context-fsm
               puml-context-fsm-event-source
               puml-context-module
               puml-context-keep-going?
               puml-context-resolved-procedures
               puml-context-unresolved-procedures
               puml-context-print-resolver-status
               resolve-procedure
               stanza->list-of-symbols
               action:add-description
               action:add-state-transition
               action:process-state-description
               action:check-start-tag
               action:check-end-tag
               action:no-start-tag-error
               action:unexpected-end-of-file-error)
  #:export (puml->fsm
            puml-string->fsm))


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
     (exit-action  . ,action:check-end-tag)
     (transitions
      (,guard:eof-object?     ,action:no-op     #f)
      (,guard:newline?        ,action:no-op     #f)
      (,guard:space?          ,action:no-op     #f)
      (,guard:#t              ,action:store     read-end-tag)))
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


;; Read state machine description in PlantUML format from a PORT and parse it.
;;
;; Required parameters:
;;   port               A port from which PlantUML data is read.
;;
;; Keyword parameters:
;;   module             A list of modules that needed to resolve output FSM
;;                      procedures.
;;                      Default value: (current-module)
;;   default-event-source
;;                      Default event source procedure name (as a symbol) that
;;                      will be set for the output FSM.
;;                      Default value: 'event-source
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
                    (default-event-source 'event-source)
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
                              #:port             port
                              #:module           module
                              #:fsm-event-source default-event-source
                              #:keep-going?      keep-going?)))
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

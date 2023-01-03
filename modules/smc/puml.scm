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
  #:use-module (smc core set)
  #:use-module (smc context char-context)
  #:use-module (smc puml-context)
  #:use-module (smc puml-fsm)
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
                    (default-event-source 'char-context-event-source)
                    (keep-going? #f)
                    (debug-mode? #f))
  (log-use-stderr! debug-mode?)
  (let* ((reader-fsm
          (make <puml-fsm>
            #:debug-mode? debug-mode?
            #:event-source     char-context-event-source))
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

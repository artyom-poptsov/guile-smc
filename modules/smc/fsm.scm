;;; fsm.scm -- Finite State Machine facilities for Guile-SMC.

;; Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Guile-SMC types and procedures for finite state machines.


;;; Code:


(define-module (smc fsm)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module ((ice-9 format)
                #:prefix ice-9:)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:use-module (smc core transition)
  #:use-module (smc core state)
  #:use-module (smc core set)
  #:export (<fsm>
            fsm?
            fsm-description
            fsm-description-set!
            fsm-event-source
            fsm-event-source-set!
            fsm-event-source-anonymous?
            fsm-debug-mode-set!
            fsm-debug-mode?
            fsm-transition-table
            fsm-transition-table-set!
            fsm-transition-add!
            fsm-state-add!
            fsm-state
            fsm-state-description-add!
            fsm-current-state
            fsm-current-state-set!
            fsm-step!
            fsm-run!
            fsm-statistics
            fsm-pretty-print-statistics
            fsm-procedures
            fsm-pre-action
            fsm-pre-action-set!
            fsm-post-action
            fsm-post-action-set!

            fsm-parent
            fsm-parent-set!
            fsm-parent-context
            fsm-parent-context-set!

            fsm-state-count
            fsm-transition-count
            fsm-incoming-transition-count
            fsm-state-reachable?
            fsm-validate

            transition-list->hash-table
            hash-table->transition-list

            fsm-log-transition
            fsm-log-error

            anonymous-procedure?))



(define-class-with-docs <fsm> ()
  "The main class that describes a finite state machine (FSM)."

  ;; Optional custom human-readable description for the finite-state machine.
  ;;
  ;; <string>
  (description
   #:init-value   #f
   #:init-keyword #:description
   #:getter       fsm-description
   #:setter       fsm-description-set!)

  ;; Is the debug mode enabled?
  ;;
  ;; <boolean>
  (debug-mode?
   #:init-value   #f
   #:init-keyword #:debug-mode?
   #:setter       fsm-debug-mode-set!
   #:getter       fsm-debug-mode?)

  ;; A transition table in the following form:
  ;;
  ;;   key    | value         |
  ;;   -------+---------------+
  ;;   state1 | #<state ...>  |
  ;;   state2 | #<state ...>  |
  ;;   ...    | ...           |
  ;;   stateN | #<state ...>  |
  ;;
  ;; <hash-table>
  (transition-table
   #:getter     fsm-transition-table
   #:setter     fsm-transition-table-set!
   #:init-keyword #:transition-table
   #:init-thunk (lambda () (make-hash-table)))

  ;; Global state machine event source.
  ;;
  ;; <procedure>
  (event-source
   #:init-value   (lambda (context) #t)
   #:init-keyword #:event-source
   #:getter       fsm-event-source
   #:setter       fsm-event-source-set!)

  ;; Global action that is performed on a context and event before all the
  ;; other guards and actions.
  ;;
  ;; <procedure>
  (pre-action
   #:init-value   (lambda (context event) context)
   #:init-keyword #:pre-action
   #:getter       fsm-pre-action
   #:setter       fsm-pre-action-set!)

  ;; Global action that is performed on a context and event after all the
  ;; other guards and actions are executed in a chain.
  ;;
  ;; <procedure>
  (post-action
   #:init-value   (lambda (context event) context)
   #:init-keyword #:post-action
   #:getter       fsm-post-action
   #:setter       fsm-post-action-set!)

  ;; REQUIRED.  Name of the current state.
  ;;
  ;; <state>
  (current-state
   #:init-keyword #:current-state
   #:init-value   #f
   #:getter       fsm-current-state
   #:setter       fsm-current-state-set!)

  ;; Counts the number of steps performed by the FSM during the execution.
  ;;
  ;; <number>
  (step-counter
   #:init-value   0
   #:getter       fsm-step-counter
   #:setter       fsm-step-counter-set!)

  ;; Counts the number of actual transitions between different states
  ;; performed by the FSM during the execution.
  ;;
  ;; <number>
  (transition-counter
   #:init-value   0
   #:getter       fsm-transition-counter
   #:setter       fsm-transition-counter-set!)

  ;; If this FSM is generated by another FSM, this slot contains a reference
  ;; to the parent FSM.
  ;;
  ;; <fsm> | #f
  (parent-fsm
   #:init-value   #f
   #:getter       fsm-parent
   #:setter       fsm-parent-set!)

  ;; If this FSM is generated by another FSM, this slot contains a reference
  ;; to the parent context.
  ;;
  ;; <context> | #f
  (parent-context
   #:init-value   #f
   #:getter       fsm-parent-context
   #:setter       fsm-parent-context-set!))

(define-method-with-docs (fsm? object)
  "Check if an OBJECT is an instance of <fsm> class."
  (is-a? object <fsm>))


;;; Error reporting.

(define %fsm-error 'fsm-error)

(define fsm-error
  (case-lambda
    ((message)
     (log-error message)
     (throw %fsm-error message))
    ((message . args)
     (apply log-error message args)
     (throw %fsm-error (apply format #f message args) args))))



(define-method (fsm-log-transition (from <state>) (to <state>))
  (log-debug "[~a] -> [~a]" (state-name from) (state-name to)))

(define-method (fsm-log-transition (from <state>) (to <state>) event)
  (log-debug "[~a] -> [~a]: ~a" (state-name from) (state-name to) event))

(define-method (fsm-log-transition (from <state>) (to <symbol>))
  (log-debug "[~a] -> [~a]" (state-name from) to))

(define-method (fsm-log-transition (from <state>) (to <boolean>))
  (log-debug "[~a] -> [*]" (state-name from)))

(define-method (fsm-log-transition (from <boolean>) (to <state>))
  (log-debug "[*] -> [~a]" (state-name to)))

(define (fsm-log-error state fmt . args)
  (apply log-error (string-append "[~a]: ERROR: " fmt) args))



(define-method (%step-counter-increment! (self <fsm>))
  (fsm-step-counter-set! self (+ (fsm-step-counter self) 1)))

(define-method (%transition-counter-increment! (self <fsm>))
  (fsm-transition-counter-set! self (+ (fsm-transition-counter self) 1)))

(define-method-with-docs (fsm-statistics (self <fsm>))
  "Get an alist of FSM statistics."
  (list (cons 'step-counter       (fsm-step-counter self))
        (cons 'transition-counter (fsm-transition-counter self))))

(define (fsm-pretty-print-statistics fsm port)
  "Print FSM statistics in human-readable format to a PORT."
  (display ";;; Statistics:\n" port)
  (for-each (lambda (record)
              (ice-9:format port ";;;   ~20,,a ~10,,@a~%"
                            (format #f "~a:" (car record))
                            (cdr record)))
            (fsm-statistics fsm)))



(define-method (%display (self <fsm>) (port <port>))
  (format port "#<fsm current-state: ~a statistics: ~a/~a ~a>"
          (and (fsm-current-state self)
               (state-name (fsm-current-state self)))
          (fsm-step-counter self)
          (fsm-transition-counter self)
          (object-address/hex-string self)))

(define-method (display (self <fsm>) (port <port>))
  (%display self port))

(define-method (write (self <fsm>) (port <port>))
  (%display self port))



(define (anonymous-procedure? x)
  "Check if X is an anonymous procedure."
  (and (procedure? x) (not (procedure-name x))))

(define (fsm-event-source-anonymous? fsm)
  "Check if an FSM instance has an anonymous event source."
  (let ((es (fsm-event-source fsm)))
    (and es
         (anonymous-procedure? es))))



(define-method-with-docs (fsm-state-add! (self  <fsm>)
                                         (state <state>))
  "Add a new state to the SM table."
  (let ((name (state-name state)))
    (unless (fsm-state self name)
      (hash-set! (fsm-transition-table self)
                 name
                 state))))

(define-method-with-docs (fsm-state (self <fsm>)
                                    (name <symbol>))
  "Get a FSM state from the transition table of SELF by a NAME."
  (hash-ref (fsm-transition-table self) name))



(define-method-with-docs (transition-list->hash-table (fsm <fsm>)
                                                      (transition-list <list>))
  "Convert a TRANSITION-LIST to a hash table."
  (let ((table (make-hash-table)))
    (for-each (lambda (transition)
                (when (hash-ref table (state:name transition))
                  (fsm-error "Duplicate state: ~a" (state:name transition)))
                (let ((state (list->state transition)))
                  (unless (state-has-event-source? state)
                    (state-event-source-set! state (fsm-event-source fsm)))
                  (hash-set! table (state-name state) state)))
              transition-list)
    (hash-map->list (lambda (state-name state)
                      (let ((tr-table
                             (map (lambda (tr)
                                    (list
                                     (transition:guard  tr)
                                     (transition:action tr)
                                     (hash-ref table
                                               (transition:next-state tr))))
                                  (state-transitions state))))
                        (state-transitions-set! state tr-table)))
                    table)
    table))

;; Convert a hash table to a transition list of the following form:
;;
;;   '(((name        . state1)
;;      (description . "description")
;;      (transitions . ((guard-procedure      action-procedure      next-state)
;;                      (guard-procedure      action-procedure      next-state)
;;                      ...
;;                      (guard-procedure      action-procedure      next-state))))
;;     (state1 ...))
;;
;; Return the transition list.
(define-method-with-docs (hash-table->transition-list table)
  "Convert a hash TABLE to a transition list."
  (hash-map->list (lambda (state-name state) (state->list state))
                  table))



(define-method (initialize (self <fsm>) initargs)
  (next-method)
  (let ((states (and (memq #:states initargs)
                     (cadr (memq #:states initargs)))))

    (when states
      (for-each (lambda (state)
                  (fsm-state-add! self state))
                states)
      (fsm-current-state-set! self (car states))))

  (let ((table (and (memq #:transition-table initargs)
                    (cadr (memq #:transition-table initargs)))))
    (when table
      (cond
       ((list? table)
        (fsm-transition-table-set! self (transition-list->hash-table self table))
        (fsm-current-state-set! self (fsm-state self (state:name (car table)))))
       (else
        (fsm-error "Transition table must be a list: ~a" table))))))



(define-method-with-docs (fsm-transition-add! (self            <fsm>)
                                              (state-name      <symbol>)
                                              (tguard          <procedure>)
                                              (action          <procedure>)
                                              (next-state-name <top>))
  "Add a new transition from a STATE-NAME to a NEXT-STATE-NAME, guarded by a
TGUARD with the specified transition ACTION.

STATE-NAME must be a valid state name that already present in the SELF FSM,
otherwise an error will be thrown.

NEXT-STATE-NAME must be either a name of a state that is present in the FSM
transition table, or #f (which means the end transition.)"
  (let ((state      (fsm-state self state-name))
        (next-state (and next-state-name
                         (fsm-state self next-state-name))))

    (unless state
      (fsm-error "fsm-transition-add!: Source state ~a is not found" state-name))

    (when (and next-state-name (not next-state))
      (fsm-error "fsm-transition-add!: Next state ~a is not found" next-state-name))

    (state-transition-add! state tguard action next-state)))

(define-method-with-docs (fsm-state-description-add! (self        <fsm>)
                                                     (state-name  <symbol>)
                                                     (description <string>))
  "Add a DESCRIPTION to the state specified by a STATE-NAME."
  (let ((trimmed-description (string-trim-both description)))
    (if (fsm-state self state-name)
        (state-description-set! (fsm-state self state-name)
                                trimmed-description)
        (fsm-state-add! self (make <state>
                               #:name state-name
                               #:description trimmed-description)))))



(define-method-with-docs (%handle-state-transition! (fsm <fsm>)
                                                    old-state
                                                    new-state
                                                    new-context)
  "Handle a state transition for an FSM in the NEW-CONTEXT of the state
machine. The transition changes an OLD-STATE to a NEW-STATE.

The procedure returns two values: new-state and new-context"
  (%step-counter-increment! fsm)
  (fsm-current-state-set! fsm new-state)
  (if (equal? old-state new-state)
      (values new-state new-context)
      (begin
        (when (fsm-debug-mode? fsm)
          (fsm-log-transition old-state new-state))
        (if new-state
            (begin
              (%transition-counter-increment! fsm)
              (values new-state
                      ((state-entry-action new-state)
                       ((state-exit-action old-state) new-context))))
            (values new-state
                    ((state-exit-action old-state) new-context))))))

(define-method-with-docs (fsm-step! (self <fsm>) event context)
  "Perform a single FSM step on the specified EVENT and a CONTEXT."
  (let ((state (fsm-current-state self)))
    (receive (next-state new-context)
        (state-run state event context)
      (%handle-state-transition! self state next-state new-context))))

(define-method-with-docs (fsm-run! (fsm          <fsm>)
                                   (event-source <procedure>)
                                   context)
  "Run an FSM with the given EVENT-SOURCE and a CONTEXT and return the new
context. EVENT-SOURCE must be a procedure that accepts a CONTEXT as a single
parameter. CONTEXT can be any Scheme object.

Return the CONTEXT after FSM execution."
  (if (fsm-current-state fsm)
      (let ((pre-action  (fsm-pre-action fsm))
            (post-action (fsm-post-action fsm)))
        (fsm-log-transition #f (fsm-current-state fsm))
        (let loop ((context ((state-entry-action (fsm-current-state fsm)) context)))
          (let ((event (event-source context)))
            (receive (new-state new-context)
                (fsm-step! fsm event (pre-action context event))
            (if new-state
                (loop (post-action new-context event))
                new-context)))))
      context))

(define-method-with-docs (fsm-run! (fsm <fsm>) context)
  "This version of the 'fsm-run!' procedure uses event sources specific for
each state."
  (if (fsm-current-state fsm)
      (let ((pre-action  (fsm-pre-action fsm))
            (post-action (fsm-post-action fsm)))
        (fsm-log-transition #f (fsm-current-state fsm))
        (let loop ((context ((state-entry-action (fsm-current-state fsm)) context)))
          (let ((event ((state-event-source (fsm-current-state fsm)) context)))
            (receive (new-state new-context)
                (fsm-step! fsm event (pre-action context event))
              (if new-state
                  (loop (post-action new-context event))
                  new-context)))))
      context))



(define-method-with-docs (fsm-state-count (self <fsm>))
  "Calculate the number of states in a finite state machine SELF. Return the
number of states."
  (hash-count (const #t) (fsm-transition-table self)))

(define-method-with-docs (fsm-transition-count (self <fsm>))
  "Calculate the total transition count for a finite state machine SELF.
Return the number of transitions."
  (hash-fold (lambda (name state result)
               (+ result (state-transition-count state)))
             0
             (fsm-transition-table self)))

(define* (fsm-incoming-transition-count self
                                        state
                                        #:key (include-recurrent-links? #f))
  "Calculate the incoming transition count for a STATE. Optionally the
procedure can include recurrent links of a STATE to itself in the calculation
if INCLUDE-RECURRENT-LINKS? is set to #t."
  (hash-fold (lambda (name other-state result)
               (log-debug "  name:             ~a" name)
               (log-debug "  other-state:      ~a" other-state)
               (log-debug "  transition-count: ~a"
                          (state-transition-count other-state state))
               (if (and (equal? (state-name state) (state-name other-state))
                        (not include-recurrent-links?))
                   (begin
                     (log-debug "    skip: ~a" other-state)
                     result)
                   (+ result (state-transition-count other-state state))))
             0
             (fsm-transition-table self)))

(define-method-with-docs (fsm-state-reachable? (self <fsm>) (state <state>))
  "Check if a STATE is reachable in the SELF finite state machine."
  (or (equal? (fsm-current-state self) state)
      (> (fsm-incoming-transition-count self state) 0)))

(define-method-with-docs (fsm-validate (self <fsm>))
  "Validate the finite state machine and return the list of errors. If the
list is empty then no errors were found."
  (let ((errors '()))
    (log-debug "fsm-validate: begin ...")
    (hash-map->list (lambda (state-name state)
                      (log-debug "state: ~a" state)
                      (unless (fsm-state-reachable? self state)
                        (set! errors (cons (cons state-name "State is not reachable")
                                           errors)))
                      (when (state-dead-end? state)
                        (set! errors (cons (cons state-name "Potentially a dead-end state.")
                                           errors))))
                    (fsm-transition-table self))
    (log-debug "fsm-validate ... done")
    errors))

(define-method-with-docs (fsm-procedures (self <fsm>))
  "Get the list of procedures involved in SELF FSM."
  (let ((procs (make <set>)))
    (hash-map->list (lambda (state-name state)
                      (set-add! procs (fsm-event-source self))
                      (set-add! procs (state-entry-action state))
                      (set-add! procs (state-exit-action state))
                      (for-each (lambda (tr)
                                  (set-add! procs (transition:guard  tr))
                                  (set-add! procs (transition:action tr)))
                                (state-transitions state)))
                    (fsm-transition-table self))
    (set-content procs)))

;;; fsm.scm ends here.


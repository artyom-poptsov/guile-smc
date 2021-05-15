;;; fsm.scm -- Finite State Machine facilities for Guile-SMC.

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

;; Guile-SMC types and procedures for finite state machines.


;;; Code:


(define-module (smc fsm)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (smc core log)
  #:use-module (smc core state)
  #:export (<fsm>
            fsm?
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
            fsm-run!
            fsm-statistics

            fsm-state-count
            fsm-transition-count
            fsm-incoming-transition-count
            fsm-state-reachable?
            fsm-validate

            transition-list->hash-table
            hash-table->transition-list

            fsm-log-transition
            fsm-log-error))


;; The main class that describes a finite state machine.
(define-class <fsm> ()
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
   #:init-value (make-hash-table))

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
   #:setter       fsm-transition-counter-set!))

;; Check if an OBJECT is an instance of <fsm> class.
(define-method (fsm? object)
  (is-a? object <fsm>))



(define-method (fsm-log-transition (from <state>) (to <state>))
  (log-debug "[~a] -> [~a]" (state-name from) (state-name to)))

(define-method (fsm-log-transition (from <state>) (to <state>) event)
  (log-debug "[~a] -> [~a]: ~a" (state-name from) (state-name to) event))

(define-method (fsm-log-transition (from <state>) (to <symbol>))
  (log-debug "[~a] -> [~a]" (state-name from) to))

(define-method (fsm-log-transition (from <state>) (to <boolean>))
  (log-debug "[~a] -> [*]" (state-name from)))

(define (fsm-log-error state fmt . args)
  (apply log-error (string-append "[~a]: " fmt) args))



(define-method (%step-counter-increment! (self <fsm>))
  (fsm-step-counter-set! self (+ (fsm-step-counter self) 1)))

(define-method (%transition-counter-increment! (self <fsm>))
  (fsm-transition-counter-set! self (+ (fsm-transition-counter self) 1)))

;; Get an alist of FSM statistics.
(define-method (fsm-statistics (self <fsm>))
  (list (cons 'step-counter       (fsm-step-counter self))
        (cons 'transition-counter (fsm-transition-counter self))))



(define-method (%display (self <fsm>) (port <port>))
  (format port "#<fsm current-state: ~a statistics: ~a/~a ~a>"
          (and (fsm-current-state self)
               (state-name (fsm-current-state self)))
          (fsm-step-counter self)
          (fsm-transition-counter self)
          (number->string (object-address self) 16)))

(define-method (display (self <fsm>) (port <port>))
  (%display self port))

(define-method (write (self <fsm>) (port <port>))
  (%display self port))



;; Add a new state to the SM table.
(define-method (fsm-state-add! (self  <fsm>)
                               (state <state>))
  (let ((name (state-name state)))
    (unless (fsm-state self name)
      (hash-set! (fsm-transition-table self)
                 name
                 state))))

;; Get a FSM state from the transition table of SELF by a NAME.
(define-method (fsm-state (self <fsm>)
                          (name <symbol>))
  (hash-ref (fsm-transition-table self) name))



;; Convert a TRANSITION-LIST to a hash table.
(define-method (transition-list->hash-table (transition-list <list>))
  (let ((table (make-hash-table)))
    (for-each (lambda (transition)
                (let* ((state-name (car transition))
                       (state-desc (and (> (length transition) 1)
                                        (string? (cadr transition))
                                        (cadr transition)))
                       (state      (hash-ref table state-name)))
                  (when state
                    (log-error "Duplicate state: ~a" state-name)
                    (error "Duplicate state" state-name))
                  (hash-set! table
                             state-name
                             (make <state>
                               #:name        state-name
                               #:description state-desc
                               #:transitions (if state-desc
                                                 (cddr transition)
                                                 (cdr transition))))))
              transition-list)
    (hash-map->list (lambda (state-name state)
                      (let ((tr-table
                             (map (lambda (tr)
                                    (let ((tguard     (list-ref tr 0))
                                          (action     (list-ref tr 1))
                                          (next-state (list-ref tr 2)))
                                      (list tguard
                                            action
                                            (hash-ref table next-state))))
                                  (state-transitions state))))
                        (state-transitions-set! state tr-table)))
                    table)
    table))

;; Convert a hash table to a transition list of the following form:
;;
;;   '((state1
;;      "description"
;;      (guard-procedure        action-procedure        next-state)
;;      (guard-procedure        action-procedure        next-state)
;;      ...
;;      (guard-procedure        action-procedure        next-state))
;;     (state1 ...))
;;
;; Return the transition list.
(define-method (hash-table->transition-list table)
  (hash-map->list (lambda (state-name state)
                    (if (null? (state-transitions state))
                        (if (state-description state)
                            (list state-name (state-description state))
                            (list state-name))
                        (if (state-description state)
                            (list state-name
                                  (state-description state)
                                  (state-transitions state))
                            (cons state-name
                                  (state-transitions state)))))
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
        (fsm-transition-table-set! self (transition-list->hash-table table))
        (fsm-current-state-set! self (fsm-state self (caar table))))
       (else
        (log-error "Transition table must be a list: ~a" table)
        (error "Transition table must be a list"))))))



(define-generic fsm-transition-add!)

;; Add a new transition to a NEXT-STATE, guarded by a TGUARD with the
;; specified transition ACTION.
(define-method (fsm-transition-add! (self       <fsm>)
                                    (state      <state>)
                                    (tguard     <procedure>)
                                    (action     <procedure>)
                                    next-state)
  (state-transition-add! state
                         tguard
                         action
                         (and next-state
                              (cond
                               ((symbol? next-state)
                                (fsm-state self next-state))
                               ((state? next-state)
                                next-state)))))

(define-method (fsm-transition-add! (self       <fsm>)
                                    (state-name <symbol>)
                                    (tguard     <procedure>)
                                    (action     <procedure>)
                                    next-state)

  (unless (fsm-state self state-name)
    (fsm-state-add! self (make <state> #:name state-name)))

  (when (and next-state (not (fsm-state self next-state)))
    (fsm-state-add! self (make <state> #:name next-state)))

  (fsm-transition-add! self (fsm-state self state-name) tguard action next-state))


(define-method (fsm-transition-add! (self       <fsm>)
                                    (state-name <symbol>)
                                    (transitions <list>))
  (for-each (lambda (transition)
              (fsm-transition-add! self
                                   state-name
                                   (list-ref transition 0)
                                   (list-ref transition 1)
                                   (list-ref transition 2)))
            transitions))

(define-method (fsm-state-description-add! (self        <fsm>)
                                           (state-name  <symbol>)
                                           (description <string>))
  (let ((trimmed-description (string-trim-both description)))
    (if (fsm-state self state-name)
        (state-description-set! (fsm-state self state-name)
                                trimmed-description)
        (fsm-state-add! self (make <state>
                               #:name state-name
                               #:description trimmed-description)))))



;; Perform a single FSM step on the specified EVENT and a CONTEXT.
(define-method (fsm-run! (self <fsm>) event context)
  (let ((state (fsm-current-state self)))
    (if state
        (let ((state (if (symbol? state)
                         (fsm-state self state)
                         state)))
          (receive (next-state new-context)
              (state-run state event context)
            (when (fsm-debug-mode? self)
              (fsm-log-transition state next-state))
            (%step-counter-increment! self)
            (unless (equal? state next-state)
              (%transition-counter-increment! self))
            (fsm-current-state-set! self next-state)
            (values next-state new-context)))
        (values #f context))))



;; Calculate the number of states in a finite state machine SELF. Return the
;; number of states.
(define-method (fsm-state-count (self <fsm>))
  (hash-count (const #t) (fsm-transition-table self)))

;; Calculate the total transition count for a finite state machine SELF. Return
;; the number of transitions.
(define-method (fsm-transition-count (self <fsm>))
  (hash-fold (lambda (name state result)
               (+ result (state-transition-count state)))
             0
             (fsm-transition-table self)))

;; Calculate the incoming transition count for a STATE. Optionally the procedure
;; can include recurrent links of a STATE to itself in the calculation if
;; INCLUDE-RECURRENT-LINKS? is set to #t.
(define* (fsm-incoming-transition-count self state
                                        #:key (include-recurrent-links? #f))
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

;; Check if a STATE is reachable in the SELF finite state machine.
(define-method (fsm-state-reachable? (self <fsm>) (state <state>))
  (or (equal? (fsm-current-state self) state)
      (> (fsm-incoming-transition-count self state) 0)))

;; Validate the finite state machine and return the list of errors. If the list
;; is empty then no errors were found.
(define-method (fsm-validate (self <fsm>))
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
    errors))

;;; fsm.scm ends here.


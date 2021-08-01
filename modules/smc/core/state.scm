;;; state.scm -- An FSM state.

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

;; An abstract FSM state implementation.


;;; Code:

(define-module (smc core state)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 receive)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:use-module (smc core transition)
  #:export (<state>
            state?
            state-name
            state-run
            state-event-source
            state:event-source/name
            state-has-event-source?
            state-event-source-set!
            state-description
            state-description-set!
            state-transition-count
            state-transition-count/foreign
            state-transition-add!
            state-transitions
            state-transitions-set!
            state-final-transitions
            state-recurrent-links-count
            state-has-final-transitions?
            state-has-recurrent-links?
            state-dead-end?

            state:name
            state:description
            state:event-source
            state:transitions

            list->state
            state->list))


;; This class describes an FSM state.
(define-class <state> ()
  ;; REQUIRED. This slot contains the state name as a <symbol>.
  ;;
  ;; <symbol>
  (name
   #:accessor     state-name
   #:init-value   #f
   #:init-keyword #:name)

  ;; <string>
  (description
   #:getter       state-description
   #:setter       state-description-set!
   #:init-keyword #:description
   #:init-value   #f)

  ;; A procedure that returns an event.
  ;;
  ;; It called by the FSM the state belongs to as follows:
  ;;   (proc context)
  ;;
  ;; <procedure>
  (event-source
   #:getter       state-event-source
   #:setter       state-event-source-set!
   #:init-keyword #:event-source
   #:init-value   #f)


  ;; <list> of transitions.
  (transitions
   #:getter       state-transitions
   #:setter       state-transitions-set!
   #:init-value   '()
   #:init-keyword #:transitions))



(define-method (%display (self <state>) (port <port>))
  (format port "#<state ~a~a ~a>"
          (state-name self)
          (if (state-description self)
              (string-append ": " (state-description self))
              "")
          (object-address/hex-string self)))

(define-method (display (self <state>) (port <port>))
  (%display self port))

(define-method (write (self <state>) (port <port>))
  (%display self port))



(define-method (state? x)
  (is-a? x <state>))

(define-method (equal? (state-1 <state>) (state-2 <state>))
  (equal? (state-name state-1) (state-name state-2)))

;; Special version of procedure that return the symbol itself.
(define-method (state-name (state <symbol>))
  state)

(define-method (state-has-event-source? (state <state>))
  (not (equal? (state-event-source state) #f)))



(define-method (state-transition-add! (self       <state>)
                                      (tguard     <procedure>)
                                      (action     <procedure>)
                                      next-state)
  (state-transitions-set! self
                          (transition-table-append (state-transitions self)
                                                   tguard action next-state)))



(define-generic state-transition-count)

(define-method (state-transition-count (self <state>))
  (length (state-transitions self)))

(define-method (state-transition-count (self <state>) to)
  (let ((to-name (state-name to)))
    (transition-table-count
     (lambda (transition)
       (equal? (state-name (transition:next-state transition))
               to-name))
     (state-transitions self))))

(define-method (state-transition-count/foreign (self <state>))
  (- (state-transition-count self)
     (state-recurrent-links-count self)))

;; Returns the number of recurrent links that the state SELF has. A recurrent
;; link is a transition of state to itself.
(define-method (state-recurrent-links-count (self <state>))
  (let ((from (state-name self)))
    (transition-table-count
     (lambda (transition)
       (equal? from (state-name (transition:next-state transition))))
     (state-transitions self))))

;; Check if the state SELF has any recurrent links (that is, transitions to
;; itself.)
(define-method (state-has-recurrent-links? (self <state>))
  (> (state-recurrent-links-count self) 0))

;; Get the number of final transitions for a state SELF.
(define-method (state-final-transitions (self <state>))
  (transition-table-count (lambda (tr) (equal? (transition:next-state tr) #f))
                         (state-transitions self)))

;; Check if a state SELF has any final transitions.
(define-method (state-has-final-transitions? (self <state>))
  (> (state-final-transitions self) 0))

;; Check if a state SELF is a dead-end state. A state is considered a dead-end
;; if it has no foreign transitions, has recurrent links and has no final
;; transitions.
(define-method (state-dead-end? (self <state>))
  (and (not (state-has-final-transitions? self))
       (> (state-recurrent-links-count self) 0)
       (zero? (state-transition-count/foreign self))))



;; Returns two values: next state (or #f) and new context.
(define-method (state-run (self <state>) event context)
  (transition-table-run (state-transitions self) event context))

;; Run a STATE in a given CONTEXT.  This procedure uses internal event source
;; of a STATE, specified by the 'event-source' slot.
(define-method (state-run (state <state>) context)
  (state-run state
             ((state-event-source state) context)
             context))


;; State serialized to an associative list of the following form:
;;
;;   `((name         . state-name)
;;     (description  . "State description")
;;     (event-source . ,event-source:state-name)
;;     (transitions
;;      (,guard:...    ,action:...    next-state-name-1)
;;      (,guard:...    ,action:...    next-state-name-1)
;;      (,guard:...    ,action:...    next-state-name-2)))
;;

(define (state:name state)
  (assoc-ref state 'name))

(define (state:description state)
  (assoc-ref state 'description))

(define (state:transitions state)
  (assoc-ref state 'transitions))

(define (state:event-source state)
  (assoc-ref state 'event-source))

(define (state:event-source/name state)
  (let ((proc (assoc-ref state 'event-source)))
    (and proc
         (procedure-name proc))))



;; Convert a list LST to a <state> instance, return the new state.
(define-method (list->state (lst <list>))
  (make <state>
    #:name         (state:name         lst)
    #:event-source (state:event-source lst)
    #:description  (state:description  lst)
    #:transitions  (state:transitions  lst)))

(define-method (state->list (state <state>))
  `((name         . ,(state-name        state))
    (description  . ,(state-description state))
    (event-source . ,(state-event-source state))
    (transitions  . ,(state-transitions state))))

;;; state.scm ends here.

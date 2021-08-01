;;; transition.scm -- State transition procedures.

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

;; Procedures for working with FSM state transitions.
;;
;; A transition table example:
;;
;;   (list (list guard:some-guard action:some-action 'state1)
;;         (list guard:#t         action:some-action 'state2))
;;


;;; Code:

(define-module (smc core transition)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:export (transition:guard
            transition:action
            transition:next-state
            transition-list-count
            transition-table-run))


;; Transition accessors.

(define-method (transition:guard (transition <list>))
  (list-ref transition 0))

(define-method (transition:action (transition <list>))
  (list-ref transition 1))

(define-method (transition:next-state (transition <list>))
  (list-ref transition 2))



(define-method (transition-list-count (predicate <procedure>) (tlist <list>))
  (fold (lambda (transition prev)
          (if (predicate transition)
              (+ prev 1)
              prev))
        0
        tlist))

(define-method (transition-table-run (tlist <list>) event context)
  (let loop ((transition-alist tlist))
    (if (null? transition-alist)
        (values #f context)
        (let ((transition (car transition-alist)))
          (if ((transition:guard transition) context event)
              (values (transition:next-state transition)
                      ((transition:action transition) context event))
              (loop (cdr transition-alist)))))))

;;; transition.scm ends here.

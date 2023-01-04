;;; context.scm -- Guile-SMC finite state machine context.

;; Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This file contains an implementation of a generic context that can be used
;; with an FSM to provide a memory.
;;
;; Also a context module usually provides event sources, guards, actions and
;; other vital procedures for a FSM to work.
;;
;; Custom contexts are required for some FSMs that need store some extra state
;; (e.g. number of characters read by the parser.)
;;
;; Guile-SMC can use any Scheme object as the context for a finite-state machine.


;;; Code:

(define-module (smc context context)
  #:use-module (oop goops)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:export (<context>
            context?
            context-debug-mode?
            context-debug-mode-set!
            guard:#t
            action:no-op))



;; This class describes a generic parser context.
(define-class <context> ()

  ;; <boolean>
  (debug-mode?
   #:init-value #f
   #:init-keyword #:debug-mode?
   #:getter       context-debug-mode?
   #:setter       context-debug-mode-set!))

(define-method (context? x)
  "Check if an X is a <context> instance."
  (is-a? x <context>))



(define-method (display (context <context>) (port <port>))
  (format port
          "#<context ~a>"
          (object-address/hex-string context)))

(define-method (write (context <context>) (port <port>))
  (display context port))



(define (action:no-op ctx event)
  ctx)

(define (guard:#t ctx event)
  "This guard is always returns #t."
  #t)

;;; context.scm ends here.

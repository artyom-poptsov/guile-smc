;;; context.scm -- Guile-SMC finite state machine context.

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
  #:use-module (smc core log)
  #:use-module (smc core stack)
  #:export (<context>
            context?
            context-debug-mode?
            context-debug-mode-set!
            context-stanza
            context-stanza-set!
            context-stanza-clear!
            context-buffer
            context-buffer-set!
            context-buffer-clear!
            context-clear!
            guard:#t
            action:no-op
            action:store
            action:clear-buffer
            action:update-stanza))

;; This class describes a generic parser context.
(define-class <context> ()
  ;; <boolean>
  (debug-mode?
   #:init-value #f
   #:init-keyword #:debug-mode?
   #:getter       context-debug-mode?
   #:setter       context-debug-mode-set!)

  ;; The buffer holds read symbols.
  ;;
  ;; <stack>
  (buffer
   #:init-thunk (lambda () (make <stack>))
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  ;; The stanza holds a logical unit of parsing (e.g. a key/value pair)
  ;;
  ;;<stack>
  (stanza
   #:init-thunk (lambda () (make <stack>))
   #:getter     context-stanza
   #:setter     context-stanza-set!))

(define-method (context? x)
  (is-a? x <context>))



(define-method (context-buffer-clear! (ctx <context>))
  (stack-clear! (context-buffer ctx)))

(define-method (context-stanza-clear! (ctx <context>))
  (stack-clear! (context-stanza ctx)))

(define-method (context-clear! (ctx <context>))
  (context-buffer-clear! ctx)
  (context-stanza-clear! ctx))



(define (action:no-op ctx event)
  ctx)

(define (action:store ctx event)
  (when (context-debug-mode? ctx)
    (log-debug "action:store: event: ~a; buffer: ~a"
               event (context-buffer ctx)))
  (stack-push! (context-buffer ctx) event)
  ctx)

;; Clear the context CTX buffer.
(define (action:clear-buffer ctx event)
  (stack-clear! (context-buffer ctx))
  ctx)

(define (action:update-stanza ctx event)
  (let ((buf    (context-buffer ctx))
        (stanza (context-stanza ctx)))
    (unless (null? buf)
      (when (context-debug-mode? ctx)
        (log-debug "action:update-stanza: event: ~a; buffer: ~a; stanza: ~a"
                   event
                   (stack-content/reversed buf)
                   (stack-content/reversed stanza)))
      (stack-push! stanza (stack-content/reversed buf))
      (stack-clear! buf))
    ctx))

(define (guard:#t ctx event)
  "This guard is always returns #t."
  #t)

;;; context.scm ends here.

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
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (smc core log)
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



(define-class-with-docs <context> ()
  "This class describes a generic parser context."

  ;; <boolean>
  (debug-mode?
   #:init-value #f
   #:init-keyword #:debug-mode?
   #:getter       context-debug-mode?
   #:setter       context-debug-mode-set!)

  ;; The buffer holds read symbols.
  ;;
  ;; <list>
  (buffer
   #:init-value '()
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  ;; The stanza holds a logical unit of parsing (e.g. a key/value pair)
  ;;
  ;;<list>
  (stanza
   #:init-value '()
   #:getter     context-stanza
   #:setter     context-stanza-set!))

(define-method (context? x)
  "Check if an X is a <context> instance."
  (is-a? x <context>))



(define-method (context-buffer-clear! (ctx <context>))
  (context-buffer-set! ctx '()))

(define-method (context-buffer-add! (ctx <context>) value)
  (context-buffer-set! ctx
                       (cons value (context-buffer ctx))))

(define-method (context-stanza-clear! (ctx <context>))
  (context-stanza-set! ctx '()))

(define-method (context-stanza-add! (ctx <context>) value)
  (context-stanza-set! ctx
                       (cons value (context-stanza ctx))))

(define-method (context-clear! (ctx <context>))
  (context-buffer-clear! ctx)
  (context-stanza-clear! ctx))



(define (action:no-op ctx event)
  ctx)

(define (action:store ctx event)
  (when (context-debug-mode? ctx)
    (log-debug "action:store: event: ~a; buffer: ~a"
               event (context-buffer ctx)))
  (context-buffer-add! ctx event)
  ctx)

(define (action:clear-buffer ctx event)
  "Clear the context CTX buffer."
  (context-buffer-clear! ctx)
  ctx)

(define (action:update-stanza ctx event)
  (let ((buf (reverse (context-buffer ctx))))
    (unless (null? buf)
      (when (context-debug-mode? ctx)
        (let ((stanza (reverse (context-stanza ctx))))
          (log-debug "action:update-stanza: event: ~a; buffer: ~a; stanza: ~a"
                     event
                     buf
                     stanza)))
      (context-stanza-add! ctx buf)
      (context-buffer-clear! ctx))
    ctx))

(define (guard:#t ctx event)
  "This guard is always returns #t."
  #t)

;;; context.scm ends here.

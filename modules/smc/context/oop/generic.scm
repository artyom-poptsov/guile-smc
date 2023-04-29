;;; generic.scm -- Guile-SMC finite state machine generic OOP context.

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

(define-module (smc context oop generic)
  #:use-module (oop goops)
  #:use-module (smc core common)
  #:export (<context>
            context?
            context-debug-mode?
            context-debug-mode-set!
            context-counter
            context-counter-set!
            context-counter-update!
            context-stanza
            context-stanza/reversed
            context-stanza-set!
            context-buffer
            context-buffer/reversed
            context-buffer-set!
            context-result
            context-result-set!
            context-result/reversed

            ;; Guards.
            buffer-empty?
            stanza-empty?
            result-empty?

            ;; Actions.
            pop-buffer
            pop-stanza
            pop-result
            clear-buffer
            clear-stanza
            clear-result
            update-counter
            push-event-to-buffer
            push-event-to-stanza
            push-event-to-result
            push-buffer-to-stanza
            push-stanza-to-result))



;; This class describes a generic parser context.
(define-class <context> ()

  ;; <boolean>
  (debug-mode?
   #:init-value #f
   #:init-keyword #:debug-mode?
   #:getter       context-debug-mode?
   #:setter       context-debug-mode-set!)

  ;; Context counter.  Can be used to count incoming events, for example.
  ;;
  ;; <number>
  (counter
   #:init-value 0
   #:getter     context-counter
   #:setter     context-counter-set!)

  ;; Context buffer to store intermediate values.
  ;;
  ;; <list>
  (buffer
   #:init-value '()
   #:init-keyword #:buffer
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  ;; Context stanza to store the chunks of intermediate context data.
  ;; This can be a logical unit of parsing (e.g. a key/value pair.)
  ;;
  ;; <list>
  (stanza
   #:init-value '()
   #:init-keyword #:stanza
   #:getter     context-stanza
   #:setter     context-stanza-set!)

  ;; The context result holds the result of the FSM work.
  ;;
  ;; <list> | <top>
  (result
   #:init-value '()
   #:init-keyword #:result
   #:getter     context-result
   #:setter     context-result-set!))



(define-method (display (context <context>) (port <port>))
  (format port
          "#<context ~a>"
          (object-address/hex-string context)))

(define-method (write (context <context>) (port <port>))
  (display context port))



(define-method (context? x)
  "Check if an X is a <context> instance."
  (is-a? x <context>))



(define-method (context-buffer/reversed (context <context>))
  (reverse (context-buffer context)))

(define-method (context-stanza/reversed (context <context>))
  (reverse (context-stanza context)))

(define-method (context-result/reversed (context <context>))
  (reverse (context-result context)))



(define-method (context-counter-update! (ctx <context>) (delta <number>))
  (context-counter-set! ctx (+ (context-counter ctx) delta)))

(define-method (context-counter-update! (ctx <context>))
  (context-counter-update! ctx 1))


;;; Guards.

(define* (buffer-empty? context #:optional event)
  (null? (context-buffer context)))

(define* (stanza-empty? context #:optional event)
  (null? (context-stanza context)))

(define* (result-empty? context #:optional event)
  (null? (context-result context)))



(define* (pop-buffer context #:optional event)
  (context-buffer-set! context (cdr (context-buffer context))))

(define* (pop-stanza context #:optional event)
  (context-stanza-set! context (cdr (context-stanza context))))

(define* (pop-result context #:optional event)
  (context-result-set! context (cdr (context-result context))))



(define* (clear-buffer context #:optional event)
  (context-buffer-set! context '())
  context)

(define* (clear-stanza context #:optional event)
  (context-stanza-set! context '())
  context)

(define* (clear-result context #:optional event)
  (context-result-set! context '())
  context)



(define* (reverse-buffer context #:optional event)
  (context-buffer-set! context (reverse (context-buffer context)))
  context)

(define* (reverse-stanza context #:optional event)
  (context-stanza-set! context (reverse (context-stanza context)))
  context)

(define* (reverse-result context #:optional event)
  (context-result-set! context (reverse (context-result context)))
  context)



(define (push-event-to-buffer context event)
  "Push a new EVENT in a CONTEXT buffer."
  (when (context-debug-mode? context)
    (log-debug "push-event-to-buffer: event: ~a; buffer: ~a"
               event
               (context-buffer context)))
  (context-buffer-set! context (cons event (context-buffer context)))
  context)

(define (push-event-to-stanza context event)
  "Push a new EVENT in a CONTEXT stanza."
  (when (context-debug-mode? context)
    (log-debug "push-event-to-stanza: event: ~a; stanza: ~a"
               event
               (context-stanza context)))
  (context-stanza-set! context (cons event (context-stanza context)))
  context)

(define (push-event-to-result context event)
  "Push a new EVENT in a CONTEXT result."
  (when (context-debug-mode? context)
    (log-debug "push-event-to-result: event: ~a; result: ~a"
               event
               (context-result context)))
  (context-result-set! context (cons event (context-result context)))
  context)

(define* (push-buffer-to-stanza context #:optional event)
  "Push the CONTEXT buffer contents to a CONTEXT stanza."
  (when (context-debug-mode? context)
    (log-debug "push-buffer-to-stanza: event: ~a; buffer: ~a; stanza: ~a"
               event
               (context-buffer context)
               (context-stanza context)))
  (if (buffer-empty? context)
      context
      (begin
        (context-stanza-set! context (cons (context-buffer/reversed context)
                                           (context-stanza context)))
        (clear-buffer context event))))

(define (push-stanza-to-result context event)
  "Push the CONTEXT stanza contents to a CONTEXT result."
  (when (context-debug-mode? context)
    (log-debug "push-buffer-to-result: event: ~a; buffer: ~a; stanza: ~a"
               event
               (context-buffer context)
               (context-stanza context)))
  (context-result-set! context (cons (context-stanza/reversed context)
                                     (context-result context)))
  (clear-stanza context event))



(define* (update-counter ctx #:optional event)
  (context-counter-update! ctx)
  ctx)

;;; generic.scm ends here.

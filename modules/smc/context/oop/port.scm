;;; port.scm -- Guile-SMC finite state machine port context.

;; Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This file contains an implementation of a port context that can be used
;; with an FSM that reads data from a port.


;;; Code:

(define-module (smc context oop port)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc context oop generic)
  #:export (<port-context>
            port-context?
            context-port
            context-counter
            context-counter-set!
            context-counter-update!
            context-stanza
            context-stanza/reversed
            context-stanza-set!
            context-stanza-add!
            context-stanza-clear!
            context-buffer
            context-buffer/reversed
            context-buffer-set!
            context-buffer-add!
            context-buffer-clear!
            context-result
            context-result-set!
            context-result/reversed
            context-clear!

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
            push-stanza-to-result
            action:update-stanza

            ;; Deprecated.
            action:clear-buffer))



(define-class <port-context> (<context>)
  ;; A port from which data is read.
  ;;
  ;; <port>
  (port
   #:init-thunk   (lambda () (current-input-port))
   #:init-keyword #:port
   #:getter       context-port)

  ;; Total number of objects read.
  ;;
  ;; <number>
  (counter
   #:init-value 0
   #:getter     context-counter
   #:setter     context-counter-set!)

  ;; The buffer holds read symbols.
  ;;
  ;; <list>
  (buffer
   #:init-value '()
   #:init-keyword #:buffer
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  ;; The stanza holds a logical unit of parsing (e.g. a key/value pair)
  ;;
  ;;<list>
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



(define (port-context? x)
  "Check if X is a <port-context> instance."
  (is-a? x <port-context>))


(define-method (context-buffer/reversed (context <port-context>))
  (reverse (context-buffer context)))

(define-method (context-stanza/reversed (context <port-context>))
  (reverse (context-stanza context)))

(define-method (context-result/reversed (context <port-context>))
  (reverse (context-result context)))



(define-method (context-counter-update! (ctx <port-context>) (delta <number>))
  (context-counter-set! ctx (+ (context-counter ctx) delta)))

(define-method (context-counter-update! (ctx <port-context>))
  (context-counter-update! ctx 1))



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


(define-method (context-buffer-clear! (ctx <port-context>))
  "Clear the context CTX buffer."
  (context-buffer-set! ctx '()))

(define-method (context-buffer-add! (ctx <port-context>) value)
  "Add a new VALUE to the context CTX buffer."
  (context-buffer-set! ctx
                       (cons value (context-buffer ctx))))

(define-method (context-stanza-clear! (ctx <port-context>))
  "Clear the context CTX stanza."
  (context-stanza-set! ctx '()))

(define-method (context-stanza-add! (ctx <port-context>) value)
  "Add a new VALUE to the context CTX stanza."
  (context-stanza-set! ctx
                       (cons value (context-stanza ctx))))

(define-method (context-clear! (ctx <port-context>))
  "Clear both the context CTX buffer and stanza."
  (context-buffer-clear! ctx)
  (context-stanza-clear! ctx))

(define* (update-counter ctx #:optional event)
  (context-counter-update! ctx))


;;; Deprecated.

(define action:clear-buffer clear-buffer)

(define (action:update-stanza ctx event)
  "Copy the context CTX buffer to the stanza, clear the buffer."
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

;;; port.scm ends here.

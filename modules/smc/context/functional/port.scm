;;; port.scm -- Guile-SMC functional port context for FSMs.

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

;; This file contains functional port context for finite-state machines.  This
;; context helps to read and parse data from a specified port.


;;; Code:

(define-module (smc context functional port)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:export (<port-context>
            make-port-context

            port-context?
            context-debug-mode?
            context-port

            context-counter
            context-counter-set
            context-counter-update
            context-buffer
            context-buffer/reversed
            context-buffer-set
            context-buffer-append
            context-stanza
            context-stanza/reversed
            context-stanza-set
            context-stanza-append
            context-result
            context-result/reversed
            context-result-set
            context-result-append

            ;; Actions.
            update-counter
            update-result
            update-stanza
            reset-buffer
            reset-stanza
            append-to-buffer

            ;; Low-level procedures.
            %make-port-context))



(define-immutable-record-type <port-context>
  (%make-port-context port debug-mode? counter buffer stanza result)
  port-context?
  (port        context-port)
  (debug-mode? context-debug-mode?)
  (counter     context-counter context-counter-set)
  (buffer      context-buffer  context-buffer-set)
  (stanza      context-stanza  context-stanza-set)
  (result      context-result  context-result-set))

(define* (make-port-context #:key
                            (port (current-input-port))
                            (debug-mode? #f)
                            (counter 0)
                            (buffer '())
                            (stanza '())
                            (result '()))
  (%make-port-context port debug-mode? counter buffer stanza result))



(set-record-type-printer!
 <port-context>
 (lambda (record port)
   (format port
           "#<port-context ~a>"
           (object-address/hex-string record))))



(define* (context-counter-update context #:key (delta 1))
  (context-counter-set context (+ (context-counter context) delta)))

(define (context-buffer/reversed context)
  (reverse (context-buffer context)))

(define (context-stanza/reversed context)
  (reverse (context-stanza context)))

(define (context-stanza-append context token)
  "Append a TOKEN to a CONTEXT stanza.  Return the updated context."
  (context-stanza-set context (cons token (context-stanza context))))

(define (context-result-append context stanza)
  "Append a STANZA to a CONTEXT result.  Return the updated context."
  (context-result-set context (cons stanza (context-result context))))

(define (context-result/reversed context)
  (reverse (context-result context)))

(define (context-buffer-append context event)
  (context-buffer-set context (cons event (context-buffer context))))


;;; Actions.
;; Those procedures are to be called from a FSM directly.

(define* (update-counter context #:optional event)
  "Increment the CONTEXT read counter.  Return the updated context.

EVENT parameter is optional and is needed only for compatibility with
Guile-SMC API."
  (context-counter-update context))

(define (append-to-buffer context event)
  "Append a DATA to a CONTEXT buffer.  Return the updated context."
  (context-buffer-append context event))

(define* (reset-buffer context #:optional event)
  "Reset a CONTEXT buffer to an empty list.  Return the updated context.

EVENT parameter is optional and is needed only for compatibility with
Guile-SMC API."
  (context-buffer-set context '()))

(define* (reset-stanza context #:optional event)
  "Reset a CONTEXT stanza to an empty list.  Return the updated context.

EVENT parameter is optional and is needed only for compatibility with
Guile-SMC API."
  (context-stanza-set context '()))

(define (update-stanza context event)
  "Propagate the data from a CONTEXT buffer to the CONTEXT stanza and reset the
buffer, ignoring an EVENT.  Return the updated context."
  (let ((buffer (context-buffer context)))
    (unless (null? buffer)
      (let ((reversed-buffer (reverse buffer)))
        (when (context-debug-mode? context)
          (let ((stanza (context-stanza/reversed context)))
            (log-debug "context-stanza-update: event: ~a; buffer: ~a; stanza: ~a"
                       event
                       buffer
                       stanza)))
        (context-buffer-reset
         (context-stanza-append context reversed-buffer))))))

(define (update-result ctx event)
  (let ((stanza (context-stanza ctx)))
    (unless (null? stanza)
      (let ((reversed-stanza (reverse buffer)))
        (when (context-debug-mode? ctx)
          (log-debug "context-result-update: event: ~a; stanza: ~a"
                     event
                     reversed-stanza))
        (context-stanza-reset
         (context-result-append ctx reversed-stanza))))))

;;; port.scm ends here.

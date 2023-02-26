;;; u8.scm -- Guile-SMC functional u8 binary context for FSMs.

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

;; This file contains functional u8 binary port context for finite-state
;; machines.  This context helps to read and parse a stream of bytes from a
;; specified port.


;;; Code:

(define-module (smc context functional u8)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 binary-ports)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:export (<u8-context>
            u8-context?
            make-u8-context

            context-debug-mode?
            context-port

            context-counter
            context-counter-set
            context-counter-update
            context-row-number
            context-row-number-set
            context-row-number-update
            context-col-number
            context-col-number-set
            context-col-number-update
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

            ;; Event source.
            next-char

            ;; Actions.
            update-counter
            update-row-number
            update-col-number
            update-result
            update-stanza
            reset-buffer
            reset-stanza
            reset-col-number
            append-to-buffer
            throw-syntax-error

            ;; Logging procedures
            context-log-error
            context-log-warning
            context-log-info
            context-log-debug

            ;; Low-level procedures.
            %make-char-context))



(define-immutable-record-type <u8-context>
  (%make-u8-context port
                    debug-mode?
                    counter
                    buffer
                    stanza
                    result)
  char-context?
  ;; <boolean>
  (debug-mode?  context-debug-mode?  context-debug-mode-set)
  ;; <port>
  (port         context-port         context-port-set)
  ;; <number>
  (counter      context-counter      context-counter-set)
  ;; <list>
  (buffer       context-buffer       context-buffer-set)
  ;; <list>
  (stanza       context-stanza       context-stanza-set)
  ;; <list>
  (result       context-result       context-result-set))

(define* (make-u8-context #:key
                            (port (current-input-port))
                            (debug-mode? #f)
                            (counter 0)
                            (buffer '())
                            (stanza '())
                            (result '()))
  (%make-u8-context port
                    debug-mode?
                    counter
                    buffer
                    stanza
                    result))



(set-record-type-printer!
 <u8-context>
 (lambda (record port)
   (format port
           "#<u8-context ~a>"
           (object-address/hex-string record))))



(define (next-u8 context)
  "Event source."
  (get-u8 (context-port context)))



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
        (reset-buffer (context-stanza-append context reversed-buffer))))))

(define (update-result ctx event)
  (let ((stanza (context-stanza ctx)))
    (unless (null? stanza)
      (let ((reversed-stanza (reverse stanza)))
        (when (context-debug-mode? ctx)
          (log-debug "context-result-update: event: ~a; stanza: ~a"
                     event
                     reversed-stanza))
        (reset-stanza
         (context-result-append ctx reversed-stanza))))))


;;; Logging.

(define (%current-position-prefix ctx)
  "Make a char context CTX prefix for a log message.  Return the prefix as a
string."
  (format #f
          "~a:~a: "
          (context-port ctx)
          (context-counter ctx)))

(define (context-log-error ctx fmt . rest)
  (apply log-error
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (context-log-warning ctx fmt . rest)
  (apply log-warning
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (context-log-info ctx fmt . rest)
  (apply log-info
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (context-log-debug ctx fmt . rest)
  (apply log-debug
         (string-append (%current-position-prefix ctx) fmt)
         rest))


;;; Error reporting.

(define (throw-syntax-error context u8)
  (error "Syntax error"
         (context-port context)
         (context-counter context)
         u8
         context))

;;; char.scm ends here.



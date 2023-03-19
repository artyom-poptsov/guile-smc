;;; char.scm -- Guile-SMC functional char context for FSMs.

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

;; This file contains functional character port context for finite-state
;; machines.  This context helps to read and parse a stream of character data
;; from a specified port.


;;; Code:

(define-module (smc context functional char)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 textual-ports)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:export (<char-context>
            char-context?
            make-char-context

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
            context-stanza
            context-stanza/reversed
            context-stanza-set
            context-result
            context-result/reversed
            context-result-set
            context-custom-data
            context-custom-data-set

            ;; Event source.
            next-char

            ;; Guards.
            buffer-empty?
            stanza-empty?
            result-empty?

            ;; Actions.
            pre-action
            clear-buffer
            clear-stanza
            clear-result
            reverse-buffer
            reverse-stanza
            reverse-result
            push-event-to-buffer
            push-event-to-stanza
            push-event-to-result
            push-buffer-to-stanza
            push-stanza-to-result
            pop-buffer
            pop-stanza
            pop-result
            update-counter
            update-row-number
            update-col-number
            reset-col-number
            throw-syntax-error

            ;; Logging procedures
            context-log-error
            context-log-warning
            context-log-info
            context-log-debug

            ;; Low-level procedures.
            %make-char-context))



(define-immutable-record-type <char-context>
  (%make-char-context port
                      debug-mode?
                      row-number
                      col-number
                      counter
                      buffer
                      stanza
                      result
                      custom-data)

  char-context?

  ;; Context port that is used to read the data.
  ;;
  ;; <port>
  (port context-port context-port-set)

  ;; Flag that specifies whether the debug mode for the context is enabled.
  ;;
  ;; <boolean>
  (debug-mode? context-debug-mode? context-debug-mode-set)

  ;; Context row counter.  Can be used to count rows that have been read.
  ;;
  ;; <number>
  (row-number   context-row-number   context-row-number-set)

  ;; Context column counter.  Can be used to indicate the current column that
  ;; have been read.
  ;;
  ;; <number>
  (col-number   context-col-number   context-col-number-set)

  ;; Context counter.  Can be used to count incoming events, for example.
  ;;
  ;; <number>
  (counter     context-counter     context-counter-set)

  ;; Context buffer to store intermediate values.
  ;;
  ;; <list>
  (buffer      context-buffer      context-buffer-set)

  ;; Context stanza to store the chunks of intermediate context data.
  ;;
  ;; <list>
  (stanza      context-stanza      context-stanza-set)

  ;; Context result to store the end result of the parser.
  ;;
  ;; <list>
  (result      context-result      context-result-set)

  ;; Context custom data that can be used by the custom contexts to store
  ;; different things such as ports or some data structures.
  (custom-data context-custom-data context-custom-data-set))

(define* (make-char-context #:key
                            (port (current-input-port))
                            (debug-mode? #f)
                            (counter 0)
                            (row-number 0)
                            (col-number 0)
                            (buffer '())
                            (stanza '())
                            (result '())
                            (custom-data '()))
  (%make-char-context port
                      debug-mode?
                      counter
                      row-number
                      col-number
                      buffer
                      stanza
                      result
                      custom-data))



(set-record-type-printer!
 <char-context>
 (lambda (record port)
   (format port
           "#<char-context ~a>"
           (object-address/hex-string record))))



(define (context-buffer/reversed context)
  (reverse (context-buffer context)))

(define (context-stanza/reversed context)
  (reverse (context-stanza context)))

(define (context-result/reversed context)
  (reverse (context-result context)))

(define* (context-counter-update context #:optional (delta 1))
  (context-counter-set context (+ (context-counter context) delta)))



(define (next-char context)
  "Event source."
  (get-char (context-port context)))


;; Guards.

(define* (buffer-empty? context #:optional event)
  (null? (context-buffer context)))

(define* (stanza-empty? context #:optional event)
  (null? (context-stanza context)))

(define* (result-empty? context #:optional event)
  (null? (context-result context)))



(define* (context-counter-update context #:key (delta 1))
  (context-counter-set context (+ (context-counter context) delta)))

(define* (context-row-number-update context #:key (delta 1))
  (context-row-number-set context (+ (context-row-number context) delta)))

(define* (context-col-number-update context #:key (delta 1))
  (context-col-number-set context (+ (context-col-number context) delta)))

(define (context-buffer/reversed context)
  (reverse (context-buffer context)))

(define (context-stanza/reversed context)
  (reverse (context-stanza context)))

(define (context-result/reversed context)
  (reverse (context-result context)))


;;; Actions.
;; Those procedures are to be called from a FSM directly.

(define* (clear-buffer context #:optional event)
  "Set the CONTEXT buffer to an empty list.  Return the updated context."
  (context-buffer-set context '()))

(define* (clear-stanza context #:optional event)
  "Set the CONTEXT stanza to an empty list.  Return the updated context."
  (context-stanza-set context '()))

(define* (clear-result context #:optional event)
  "Set the CONTEXT result to an empty list.  Return the updated context."
  (context-result-set context '()))



(define* (reverse-buffer context #:optional event)
  (context-buffer-set context (reverse (context-buffer context))))

(define* (reverse-stanza context #:optional event)
  (context-stanza-set context (reverse (context-stanza context))))

(define* (reverse-result context #:optional event)
  (context-result-set context (reverse (context-result context))))



(define (push-event-to-buffer context event)
  "Push an EVENT to the CONTEXT buffer.  Return the updated context."
  (context-buffer-set context (cons event (context-buffer context))))

(define (push-event-to-stanza context event)
  "Push an EVENT to the CONTEXT stanza.  Return the updated context."
  (context-stanza-set context (cons event (context-stanza context))))

(define (push-event-to-result context event)
  "Push an EVENT to the CONTEXT result.  Return the updated context."
  (context-result-set context (cons event (context-result context))))

(define (push-buffer-to-stanza context event)
  "Push the CONTEXT buffer content to the CONTEXT stanza and clear the CONTEXT buffer.
Return the updated context."
  (clear-buffer (context-stanza-set context
                                    (cons (context-buffer context)
                                          (context-stanza context)))
                event))

(define (push-stanza-to-result context event)
  "Push the CONTEXT stanza content to the CONTEXT result and clear the CONTEXT stanza.
Return the updated context."
  (clear-stanza context
                (context-result-set context
                                    (cons (context-stanza context)
                                          (context-result context)))))



(define* (pop-buffer context #:optional event)
  "Remove the last element of CONTEXT buffer.  Return the updated context."
  (context-buffer-set context (cdr (context-buffer context))))

(define* (pop-stanza context #:optional event)
  "Remove the last element of CONTEXT stanza.  Return the updated context."
  (context-stanza-set context (cdr (context-stanza context))))

(define* (pop-result context #:optional event)
  "Remove the last element of CONTEXT result.  Return the updated context."
  (context-result-set context (cdr (context-result context))))



(define* (update-counter context #:optional event)
  (context-counter-set context (+ (context-counter context) 1)))

(define* (update-row-number context #:optional event)
  "Increment the CONTEXT row number.  Return the updated context.

EVENT parameter is optional and is needed only for compatibility with
Guile-SMC API."
  (context-row-number-update context))

(define* (update-col-number context #:optional event)
  "Increment the CONTEXT column number.  Return the updated context.

EVENT parameter is optional and is needed only for compatibility with
Guile-SMC API."
  (context-col-number-update context))

(define* (reset-col-number context #:optional event)
  (context-col-number-set context 0))



(define (pre-action context char)
  "Update counters in a character CONTEXT based on an incoming character CHAR.
These counters are thrown when a syntax error occurred.  Return the updated
context.

This procedure can be used as a pre-action for an FSM."
  (unless (eof-object? char)
    (if (char=? char #\newline)
        (reset-col-number (update-row-number (update-counter context)))
        (update-col-number (update-row-number (update-counter context))))))


;;; Logging.

(define (%current-position-prefix ctx)
  "Make a char context CTX prefix for a log message.  Return the prefix as a
string."
  (format #f "~a:~a:~a: "
          (context-port ctx)
          (context-row-number ctx)
          (context-col-number ctx)))

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

(define (throw-error context event)
  (error "Context error" context event))

(define (throw-syntax-error context char)
  (error "Syntax error"
         (context-port context)
         (context-row-number context)
         (context-col-number context)
         char
         context))

;;; char.scm ends here.

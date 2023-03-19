;;; char.scm -- Guile-SMC finite state machine character context.

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

;; This file contains an implementation of a character context based on a
;; generic context. The character context can be used to handle a stream of
;; characters.


;;; Code:

(define-module (smc context oop char)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (smc context oop generic)
  #:use-module (smc context oop port)
  #:use-module (smc core log)
  #:re-export (context-port
               context-counter
               context-counter-update!
               context-stanza
               context-stanza/reversed
               context-stanza-set!
               context-buffer
               context-buffer/reversed
               context-buffer-set!
               context-result
               context-result-set!

               ;; Actions.
               buffer-empty?
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
               push-stanza-to-result)
  #:export (<char-context>
            char-context?
            context-row-number
            context-row-number-set!
            context-row-number-update!
            context-col-number
            context-col-number-set!
            context-col-number-update!
            char-context-update-counters!

            char-context-event-source
            char-context-pre-action

            ;; Actions.
            throw-syntax-error

            ;; All guards that are exported with 'define-public' below.

            ;; Logging procedures
            context-log-error
            context-log-warning
            context-log-info
            context-log-debug))


(define-class <char-context> (<port-context>)
  ;; Current text column number.
  ;;
  ;; <number>
  (col-number
   #:init-value 0
   #:getter     context-col-number
   #:setter     context-col-number-set!)

  ;; Current text row number.
  ;;
  ;; <number>
  (row-number
   #:init-value 0
   #:getter     context-row-number
   #:setter     context-row-number-set!))



(define (char-context? x)
  (is-a? x <char-context>))



(define* (context-row-number-update! context #:optional (delta 1))
  "Increment the current row number. Resets the current column number to
zero."
  (context-row-number-set! context (+ (context-row-number context) 1)))

(define* (context-col-number-update! context #:optional (delta 1))
  "Increment the current row number. Resets the current column number to
zero."
  (context-col-number-set! context (+ (context-col-number context) 1)))



(define (char-context-pre-action ctx ch)
  "Update counters in a character context CTX based on an incoming character CH.
These counters are thrown when a syntax error occurred.  Return the updated
context.

This procedure can be used as a pre-action for an FSM."
  (unless (eof-object? ch)
    (context-counter-update! ctx)
    (context-col-number-update! ctx)
    (when (char=? ch #\newline)
      (context-row-number-update! ctx)
      (context-col-number-set! ctx 0)))
  ctx)


;;; Event source.

(define-method (char-context-event-source (context <char-context>))
  "Get the next character from a CONTEXT port."
  (get-char (context-port context)))



(define (throw-syntax-error ctx ch)
  (error "Syntax error"
         (context-port ctx)
         (context-row-number ctx)
         (context-col-number ctx)
         ch
         ctx))



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

;;; char.scm ends here.

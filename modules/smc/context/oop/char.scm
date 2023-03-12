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
  #:re-export (;; From (smc context oop context)
               ;; and (smc context oop port)
               <context>
               context?
               context-debug-mode?
               context-debug-mode-set!
               <port-context>
               context-port
               context-counter
               context-stanza
               context-stanza-set!
               context-stanza-add!
               context-stanza-clear!
               context-buffer
               context-buffer-set!
               context-buffer-add!
               context-buffer-clear!
               context-clear!
               action:store
               action:clear-buffer
               action:update-stanza)
  #:export (<char-context>
            char-context-row
            char-context-col
            char-context-update-counters!

            char-context-event-source

            ;; Actions.
            action:syntax-error

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
   #:getter     char-context-col
   #:setter     char-context-col-set!)

  ;; Current text row number.
  ;;
  ;; <number>
  (row-number
   #:init-value 0
   #:getter     char-context-row
   #:setter     char-context-row-set!))



(define-method (%col++! (ctx <char-context>))
  "Increment the current text column."
  (char-context-col-set! ctx (+ (char-context-col ctx) 1)))

(define-method (%col-reset! (ctx <char-context>))
  "Reset the current text column."
  (char-context-col-set! ctx 0))

(define-method (%row++! (ctx <char-context>))
  "Increment the current row number. Resets the current column number to
zero."
  (char-context-row-set! ctx (+ (char-context-row ctx) 1))
  (%col-reset! ctx))

(define-method (char-context-update-counters! (ctx <char-context>) ch)
  "Update counters in a character context CTX based on an incoming character CH.
These counters are thrown when a syntax error occurred."
  (unless (eof-object? ch)
    (context-counter++! ctx)
    (%col++! ctx)
    (when (char=? ch #\newline)
      (%row++! ctx))))


;;; Event source.

(define-method (char-context-event-source (context <char-context>))
  "Get the next character from a CONTEXT port."
  (let ((ch (get-char (context-port context))))
    (char-context-update-counters! context ch)
    ch))



(define (action:syntax-error ctx ch)
  (error "Syntax error"
         (context-port ctx)
         (char-context-row ctx)
         (char-context-col ctx)
         ch
         ctx))



(define (%current-position-prefix ctx)
  "Make a char context CTX prefix for a log message.  Return the prefix as a
string."
  (format #f "~a:~a:~a: "
          (context-port ctx)
          (char-context-row ctx)
          (char-context-col ctx)))

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
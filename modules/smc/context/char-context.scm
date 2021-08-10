;;; char-context.scm -- Guile-SMC finite state machine character context.

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

;; This file contains an implementation of a character context based on a
;; generic context. The character context can be used to handle a stream of
;; characters.


;;; Code:

(define-module (smc context char-context)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (smc context context)
  #:use-module (smc core log)
  #:re-export (;; From (smc context context)
               <context>
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
               action:update-stanza)
  #:export (<char-context>
            char-context-port
            char-context-counter
            char-context-row
            char-context-col
            char-context-update-counters!

            event-source

            guard:asterisk?
            guard:equals-sign?
            guard:newline?
            guard:hyphen-minus?
            guard:space?
            guard:less-than-sign?
            guard:letter?
            guard:more-than-sign?
            guard:colon?
            guard:semicolon?
            guard:eof-object?
            guard:single-quote?
            guard:left-square-bracket?
            guard:right-square-bracket?
            guard:at-symbol?
            action:syntax-error

            ;; Logging procedures
            context-log-error
            context-log-warning
            context-log-info
            context-log-debug))

(define-class <char-context> (<context>)
  ;; A port from which data is read.
  ;;
  ;; <port>
  (port
   #:init-keyword #:port
   #:getter       char-context-port)

  ;; Total number of characters consumed.
  ;;
  ;; <number>
  (counter
   #:init-value 0
   #:getter     char-context-counter
   #:setter     char-context-counter-set!)

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



(define-method (%counter++! (ctx <char-context>))
  (char-context-counter-set! ctx (+ (char-context-counter ctx) 1)))

;; Increment the current text column.
(define-method (%col++! (ctx <char-context>))
  (char-context-col-set! ctx (+ (char-context-col ctx) 1)))

;; Reset the current text column.
(define-method (%col-reset! (ctx <char-context>))
  (char-context-col-set! ctx 0))

;; Increment the current row number. Resets the current column number to zero.
(define-method (%row++! (ctx <char-context>))
  (char-context-row-set! ctx (+ (char-context-row ctx) 1))
  (%col-reset! ctx))

;; Update counters in a character context CTX based on an incoming character
;; CH.  These counters are thrown when a syntax error occurred.
(define-method (char-context-update-counters! (ctx <char-context>) ch)
  (unless (eof-object? ch)
    (%counter++! ctx)
    (%col++! ctx)
    (when (char=? ch #\newline)
      (%row++! ctx))))


;;; Event source.

;; Get the next character from a CONTEXT port.
(define-method (event-source (context <char-context>))
  (let ((ch (get-char (char-context-port context))))
    (char-context-update-counters! context ch)
    ch))


;;; Guards.
(define (make-guard ch1)
  "Make a procedure that checks if a CH1 equals to CH2."
  (lambda (ctx ch2) (char=? ch1 ch2)))

(define (make-charset-guard charset)
  "Make a procedure that checks if a CH is in a CHARSET."
  (lambda (ctx ch) (char-set-contains? charset ch)))

(define guard:equals-sign?           (make-guard #\=))
(define guard:newline?               (make-guard #\newline))
(define guard:hyphen-minus?          (make-guard #\-))
(define guard:space?                 (make-guard #\space))
(define guard:less-than-sign?        (make-guard #\<))
(define guard:more-than-sign?        (make-guard #\>))
(define guard:letter?                (make-charset-guard char-set:letter))
(define guard:single-quote?          (make-guard #\'))
(define guard:colon?                 (make-guard #\:))
(define guard:semicolon?             (make-guard #\;))
(define guard:left-square-bracket?   (make-guard #\[))
(define guard:right-square-bracket?  (make-guard #\]))
(define guard:at-symbol?             (make-guard #\@))
(define guard:asterisk?              (make-guard #\*))

(define (guard:eof-object? ctx ch)
  (eof-object? ch))



(define (action:syntax-error ctx ch)
  (error "Syntax error"
         (char-context-port ctx)
         (char-context-row ctx)
         (char-context-col ctx)
         ch
         ctx))



(define (%current-position-prefix ctx)
  (format #f "~a:~a:~a: "
          (char-context-port ctx)
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

;;; char-context.scm ends here.

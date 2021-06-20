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
  #:use-module (oop goops)
  #:use-module (smc context context)
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
               guard:#t
               action:no-op
               action:store
               action:update-stanza)
  #:export (<char-context>
            char-context-counter
            char-context-row
            char-context-col
            char-context-update-counters!
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
            action:syntax-error))

(define-class <char-context> (<context>)
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


;;; Guards.

(define (guard:equals-sign? ctx ch)
  (char=? ch #\=))

(define (guard:newline? ctx ch)
  (char=? ch #\newline))

(define (guard:hyphen-minus? ctx ch)
  (char=? ch #\-))

(define (guard:space? ctx ch)
  (char=? ch #\space))

(define (guard:less-than-sign? ctx ch)
  (char=? ch #\<))

(define (guard:more-than-sign? ctx ch)
  (char=? ch #\>))

(define (guard:letter? ctx ch)
  (char-set-contains? char-set:letter ch))

(define (guard:eof-object? ctx ch)
  (eof-object? ch))

(define (guard:single-quote? ctx ch)
  (char=? ch #\'))

(define (guard:colon? ctx ch)
  (char=? ch #\:))

(define (guard:semicolon? ctx ch)
  (char=? ch #\;))

(define (guard:left-square-bracket? ctx ch)
  (char=? ch #\[))

(define (guard:right-square-bracket? ctx ch)
  (char=? ch #\]))

(define (guard:at-symbol? ctx ch)
  (char=? ch #\@))

(define (guard:asterisk? ctx ch)
  (char=? ch #\*))



(define (action:syntax-error ctx ch)
  (error "Syntax error"
         (char-context-row ctx)
         (char-context-col ctx)
         ch
         ctx))

;;; char-context.scm ends here.

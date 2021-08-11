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

            ;; Actions.
            action:syntax-error

            ;; All guards that are exported with 'define-public' below.

            ;; Logging procedures
            context-log-error
            context-log-warning
            context-log-info
            context-log-debug

            make-char-guard
            make-charset-guard))

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
(define (make-char-guard ch1)
  "Make a procedure that checks if a CH1 equals to CH2."
  (lambda (ctx ch2) (char=? ch1 ch2)))

(define (make-charset-guard charset)
  "Make a procedure that checks if a CH is in a CHARSET."
  (lambda (ctx ch) (char-set-contains? charset ch)))

;;; The procedures below are predicates for symbols of the ASCII table, in the
;;; same order.

(define-public guard:nul?                   (make-char-guard #\nul))
(define-public guard:soh?                   (make-char-guard #\soh))
(define-public guard:stx?                   (make-char-guard #\stx))
(define-public guard:etx?                   (make-char-guard #\etx))
(define-public guard:eot?                   (make-char-guard #\eot))
(define-public guard:enq?                   (make-char-guard #\enq))
(define-public guard:ack?                   (make-char-guard #\ack))
(define-public guard:bel?                   (make-char-guard #\bel))
(define-public guard:bs?                    (make-char-guard #\bs))
(define-public guard:tab?                   (make-char-guard #\tab))
(define-public guard:lf?                    (make-char-guard #\linefeed))
(define-public guard:vt?                    (make-char-guard #\vtab))
(define-public guard:ff?                    (make-char-guard #\ff))
(define-public guard:cr?                    (make-char-guard #\cr))
(define-public guard:so?                    (make-char-guard #\so))
(define-public guard:si?                    (make-char-guard #\si))
(define-public guard:dle?                   (make-char-guard #\dle))
(define-public guard:dc1?                   (make-char-guard #\dc1))
(define-public guard:dc2?                   (make-char-guard #\dc2))
(define-public guard:dc3?                   (make-char-guard #\dc3))
(define-public guard:dc4?                   (make-char-guard #\dc4))
(define-public guard:nak?                   (make-char-guard #\nak))
(define-public guard:syn?                   (make-char-guard #\syn))
(define-public guard:etb?                   (make-char-guard #\etb))
(define-public guard:can?                   (make-char-guard #\can))
(define-public guard:em?                    (make-char-guard #\em))
(define-public guard:sub?                   (make-char-guard #\sub))
(define-public guard:esc?                   (make-char-guard #\esc))
(define-public guard:fs?                    (make-char-guard #\fs))
(define-public guard:gs?                    (make-char-guard #\gs))
(define-public guard:rs?                    (make-char-guard #\rs))
(define-public guard:us?                    (make-char-guard #\us))
(define-public guard:space?                 (make-char-guard #\space))
(define-public guard:exclamation-mark?      (make-char-guard #\!))
(define-public guard:double-quote?          (make-char-guard #\"))
(define-public guard:number-sign?           (make-char-guard #\#))
(define-public guard:dollar-sign?           (make-char-guard #\$))
(define-public guard:percent-sign?          (make-char-guard #\%))
(define-public guard:ampersand?             (make-char-guard #\&))
(define-public guard:single-quote?          (make-char-guard #\'))
(define-public guard:left-parenthesis?      (make-char-guard #\())
(define-public guard:right-parenthesis?     (make-char-guard #\)))
(define-public guard:asterisk?              (make-char-guard #\*))
(define-public guard:plus-sign?             (make-char-guard #\+))
(define-public guard:comma?                 (make-char-guard #\,))
(define-public guard:hyphen-minus?          (make-char-guard #\-))
(define-public guard:full-stop?             (make-char-guard #\.))
(define-public guard:solidus?               (make-char-guard #\/))

;; BEGIN: Digits.
(define-public guard:digit-zero?            (make-char-guard #\0))
(define-public guard:digit-one?             (make-char-guard #\1))
(define-public guard:digit-two?             (make-char-guard #\2))
(define-public guard:digit-three?           (make-char-guard #\3))
(define-public guard:digit-four?            (make-char-guard #\4))
(define-public guard:digit-five?            (make-char-guard #\5))
(define-public guard:digit-six?             (make-char-guard #\6))
(define-public guard:digit-seven?           (make-char-guard #\7))
(define-public guard:digit-eight?           (make-char-guard #\8))
(define-public guard:digit-nine?            (make-char-guard #\9))
;; END: Digits.

(define-public guard:colon?                 (make-char-guard #\:))
(define-public guard:semicolon?             (make-char-guard #\;))
(define-public guard:less-than-sign?        (make-char-guard #\<))
(define-public guard:equals-sign?           (make-char-guard #\=))
(define-public guard:more-than-sign?        (make-char-guard #\>))
(define-public guard:question-mark?         (make-char-guard #\?))
(define-public guard:at-symbol?             (make-char-guard #\@))

;; BEGIN: Uppercase letters.
(define-public guard:letter-A?              (make-char-guard #\A))
(define-public guard:letter-B?              (make-char-guard #\B))
(define-public guard:letter-C?              (make-char-guard #\C))
(define-public guard:letter-D?              (make-char-guard #\D))
(define-public guard:letter-E?              (make-char-guard #\E))
(define-public guard:letter-F?              (make-char-guard #\F))
(define-public guard:letter-G?              (make-char-guard #\G))
(define-public guard:letter-H?              (make-char-guard #\H))
(define-public guard:letter-I?              (make-char-guard #\I))
(define-public guard:letter-J?              (make-char-guard #\J))
(define-public guard:letter-K?              (make-char-guard #\K))
(define-public guard:letter-L?              (make-char-guard #\L))
(define-public guard:letter-M?              (make-char-guard #\M))
(define-public guard:letter-N?              (make-char-guard #\N))
(define-public guard:letter-O?              (make-char-guard #\O))
(define-public guard:letter-P?              (make-char-guard #\P))
(define-public guard:letter-Q?              (make-char-guard #\Q))
(define-public guard:letter-R?              (make-char-guard #\R))
(define-public guard:letter-S?              (make-char-guard #\S))
(define-public guard:letter-T?              (make-char-guard #\T))
(define-public guard:letter-Y?              (make-char-guard #\Y))
(define-public guard:letter-V?              (make-char-guard #\V))
(define-public guard:letter-W?              (make-char-guard #\W))
(define-public guard:letter-X?              (make-char-guard #\X))
(define-public guard:letter-Y?              (make-char-guard #\Y))
(define-public guard:letter-Z?              (make-char-guard #\Z))
;; END: Uppercase letter.

(define-public guard:left-square-bracket?   (make-char-guard #\[))
(define-public guard:reverse-solidus?       (make-char-guard #\\))
(define-public guard:right-square-bracket?  (make-char-guard #\]))
(define-public guard:circumflex-accent?     (make-char-guard #\^))
(define-public guard:low-line?              (make-char-guard #\_))
(define-public guard:grave-accent?          (make-char-guard #\`))

;; BEGIN: Lowercase letters.
(define-public guard:letter-a?              (make-char-guard #\a))
(define-public guard:letter-b?              (make-char-guard #\b))
(define-public guard:letter-c?              (make-char-guard #\c))
(define-public guard:letter-d?              (make-char-guard #\d))
(define-public guard:letter-e?              (make-char-guard #\e))
(define-public guard:letter-f?              (make-char-guard #\f))
(define-public guard:letter-g?              (make-char-guard #\g))
(define-public guard:letter-h?              (make-char-guard #\h))
(define-public guard:letter-i?              (make-char-guard #\i))
(define-public guard:letter-j?              (make-char-guard #\j))
(define-public guard:letter-k?              (make-char-guard #\k))
(define-public guard:letter-l?              (make-char-guard #\l))
(define-public guard:letter-m?              (make-char-guard #\m))
(define-public guard:letter-n?              (make-char-guard #\n))
(define-public guard:letter-o?              (make-char-guard #\o))
(define-public guard:letter-p?              (make-char-guard #\p))
(define-public guard:letter-q?              (make-char-guard #\q))
(define-public guard:letter-r?              (make-char-guard #\r))
(define-public guard:letter-s?              (make-char-guard #\s))
(define-public guard:letter-t?              (make-char-guard #\t))
(define-public guard:letter-y?              (make-char-guard #\y))
(define-public guard:letter-v?              (make-char-guard #\v))
(define-public guard:letter-w?              (make-char-guard #\w))
(define-public guard:letter-x?              (make-char-guard #\x))
(define-public guard:letter-y?              (make-char-guard #\y))
(define-public guard:letter-z?              (make-char-guard #\z))
;; END: Lowercase letters.

(define-public guard:left-curly-bracket?    (make-char-guard #\{))
(define-public guard:vertical-line?         (make-char-guard #\|))
(define-public guard:right-curly-bracket?   (make-char-guard #\}))
(define-public guard:tilde?                 (make-char-guard #\~))
(define-public guard:del?                   (make-char-guard (integer->char 127)))

;; Charset predicates.
(define-public guard:letter?                (make-charset-guard char-set:letter))
(define-public guard:lower-case?            (make-charset-guard char-set:lower-case))
(define-public guard:upper-case?            (make-charset-guard char-set:upper-case))
(define-public guard:digit?                 (make-charset-guard char-set:digit))
(define-public guard:letter+digit?          (make-charset-guard char-set:letter+digit))
(define-public guard:graphic?               (make-charset-guard char-set:graphic))
(define-public guard:printing?              (make-charset-guard char-set:printing))
(define-public guard:whitespace?            (make-charset-guard char-set:whitespace))
(define-public guard:blank?                 (make-charset-guard char-set:blank))
(define-public guard:punctuation?           (make-charset-guard char-set:punctuation))
(define-public guard:symbol?                (make-charset-guard char-set:symbol))
(define-public guard:hex-digit?             (make-charset-guard char-set:hex-digit))
(define-public guard:ascii?                 (make-charset-guard char-set:ascii))

;; Misc. procedures.
(define-public guard:newline?               (make-char-guard #\newline))
(define-public (guard:eof-object? ctx ch)
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

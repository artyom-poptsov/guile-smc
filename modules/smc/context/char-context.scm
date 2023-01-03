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
  #:use-module (smc context port)
  #:use-module (smc core log)
  #:re-export (;; From (smc context context)
               ;; and (smc context port)
               <context>
               context?
               context-debug-mode?
               context-debug-mode-set!
               context-stanza
               context-stanza-set!
               context-stanza-add!
               context-stanza-clear!
               context-buffer
               context-buffer-set!
               context-buffer-add!
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



(define char-context-port context-port)
(define char-context-counter context-counter)



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
    (context-counter++! ctx)
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

;; Make a procedure that checks if a CH1 equals to CH2.
(define-syntax-rule (make-char-guard name ch1)
  (begin
    (define-method (name (ctx <char-context>) (ch2 <char>))
      (char=? ch1 ch2))
    (export name)))

;; Make a procedure that checks if a CH is in a CHARSET.
(define-syntax-rule (make-charset-guard name charset)
  (begin
    (define-method (name (ctx <char-context>) (ch <char>))
      (char-set-contains? charset ch))
    (export name)))

;;; The procedures below are predicates for symbols of the ASCII table, in the
;;; same order.

(make-char-guard guard:nul?                    #\nul)
(make-char-guard guard:soh?                    #\soh)
(make-char-guard guard:stx?                    #\stx)
(make-char-guard guard:etx?                    #\etx)
(make-char-guard guard:eot?                    #\eot)
(make-char-guard guard:enq?                    #\enq)
(make-char-guard guard:ack?                    #\ack)
(make-char-guard guard:bel?                    #\bel)
(make-char-guard guard:bs?                     #\bs)
(make-char-guard guard:tab?                    #\tab)
(make-char-guard guard:lf?                     #\linefeed)
(make-char-guard guard:vt?                     #\vtab)
(make-char-guard guard:ff?                     #\ff)
(make-char-guard guard:cr?                     #\cr)
(make-char-guard guard:so?                     #\so)
(make-char-guard guard:si?                     #\si)
(make-char-guard guard:dle?                    #\dle)
(make-char-guard guard:dc1?                    #\dc1)
(make-char-guard guard:dc2?                    #\dc2)
(make-char-guard guard:dc3?                    #\dc3)
(make-char-guard guard:dc4?                    #\dc4)
(make-char-guard guard:nak?                    #\nak)
(make-char-guard guard:syn?                    #\syn)
(make-char-guard guard:etb?                    #\etb)
(make-char-guard guard:can?                    #\can)
(make-char-guard guard:em?                     #\em)
(make-char-guard guard:sub?                    #\sub)
(make-char-guard guard:esc?                    #\esc)
(make-char-guard guard:fs?                     #\fs)
(make-char-guard guard:gs?                     #\gs)
(make-char-guard guard:rs?                     #\rs)
(make-char-guard guard:us?                     #\us)
(make-char-guard guard:space?                  #\space)
(make-char-guard guard:exclamation-mark?       #\!)
(make-char-guard guard:double-quote?           #\")
(make-char-guard guard:number-sign?            #\#)
(make-char-guard guard:dollar-sign?            #\$)
(make-char-guard guard:percent-sign?           #\%)
(make-char-guard guard:ampersand?              #\&)
(make-char-guard guard:single-quote?           #\')
(make-char-guard guard:left-parenthesis?       #\()
(make-char-guard guard:right-parenthesis?      #\))
(make-char-guard guard:asterisk?               #\*)
(make-char-guard guard:plus-sign?              #\+)
(make-char-guard guard:comma?                  #\,)
(make-char-guard guard:hyphen-minus?           #\-)
(make-char-guard guard:full-stop?              #\.)
(make-char-guard guard:solidus?                #\/)

;; BEGIN: Digits.
(make-char-guard guard:digit-zero?             #\0)
(make-char-guard guard:digit-one?              #\1)
(make-char-guard guard:digit-two?              #\2)
(make-char-guard guard:digit-three?            #\3)
(make-char-guard guard:digit-four?             #\4)
(make-char-guard guard:digit-five?             #\5)
(make-char-guard guard:digit-six?              #\6)
(make-char-guard guard:digit-seven?            #\7)
(make-char-guard guard:digit-eight?            #\8)
(make-char-guard guard:digit-nine?             #\9)
;; END: Digits.

(make-char-guard guard:colon?                  #\:)
(make-char-guard guard:semicolon?              #\;)
(make-char-guard guard:less-than-sign?         #\<)
(make-char-guard guard:equals-sign?            #\=)
(make-char-guard guard:more-than-sign?         #\>)
(make-char-guard guard:question-mark?          #\?)
(make-char-guard guard:at-symbol?              #\@)

;; BEGIN: Uppercase letters.
(make-char-guard guard:letter-A?               #\A)
(make-char-guard guard:letter-B?               #\B)
(make-char-guard guard:letter-C?               #\C)
(make-char-guard guard:letter-D?               #\D)
(make-char-guard guard:letter-E?               #\E)
(make-char-guard guard:letter-F?               #\F)
(make-char-guard guard:letter-G?               #\G)
(make-char-guard guard:letter-H?               #\H)
(make-char-guard guard:letter-I?               #\I)
(make-char-guard guard:letter-J?               #\J)
(make-char-guard guard:letter-K?               #\K)
(make-char-guard guard:letter-L?               #\L)
(make-char-guard guard:letter-M?               #\M)
(make-char-guard guard:letter-N?               #\N)
(make-char-guard guard:letter-O?               #\O)
(make-char-guard guard:letter-P?               #\P)
(make-char-guard guard:letter-Q?               #\Q)
(make-char-guard guard:letter-R?               #\R)
(make-char-guard guard:letter-S?               #\S)
(make-char-guard guard:letter-T?               #\T)
(make-char-guard guard:letter-U?               #\U)
(make-char-guard guard:letter-V?               #\V)
(make-char-guard guard:letter-W?               #\W)
(make-char-guard guard:letter-X?               #\X)
(make-char-guard guard:letter-Y?               #\Y)
(make-char-guard guard:letter-Z?               #\Z)
;; END: Uppercase letter.

(make-char-guard guard:left-square-bracket?    #\[)
(make-char-guard guard:reverse-solidus?        #\\)
(make-char-guard guard:right-square-bracket?   #\])
(make-char-guard guard:circumflex-accent?      #\^)
(make-char-guard guard:low-line?               #\_)
(make-char-guard guard:grave-accent?           #\`)

;; BEGIN: Lowercase letters.
(make-char-guard guard:letter-a?               #\a)
(make-char-guard guard:letter-b?               #\b)
(make-char-guard guard:letter-c?               #\c)
(make-char-guard guard:letter-d?               #\d)
(make-char-guard guard:letter-e?               #\e)
(make-char-guard guard:letter-f?               #\f)
(make-char-guard guard:letter-g?               #\g)
(make-char-guard guard:letter-h?               #\h)
(make-char-guard guard:letter-i?               #\i)
(make-char-guard guard:letter-j?               #\j)
(make-char-guard guard:letter-k?               #\k)
(make-char-guard guard:letter-l?               #\l)
(make-char-guard guard:letter-m?               #\m)
(make-char-guard guard:letter-n?               #\n)
(make-char-guard guard:letter-o?               #\o)
(make-char-guard guard:letter-p?               #\p)
(make-char-guard guard:letter-q?               #\q)
(make-char-guard guard:letter-r?               #\r)
(make-char-guard guard:letter-s?               #\s)
(make-char-guard guard:letter-t?               #\t)
(make-char-guard guard:letter-u?               #\u)
(make-char-guard guard:letter-v?               #\v)
(make-char-guard guard:letter-w?               #\w)
(make-char-guard guard:letter-x?               #\x)
(make-char-guard guard:letter-y?               #\y)
(make-char-guard guard:letter-z?               #\z)
;; END: Lowercase letters.

(make-char-guard guard:left-curly-bracket?     #\{)
(make-char-guard guard:vertical-line?          #\|)
(make-char-guard guard:right-curly-bracket?    #\})
(make-char-guard guard:tilde?                  #\~)
(make-char-guard guard:del?                    (integer->char 127))

;; Charset predicates.
(make-charset-guard guard:letter?              char-set:letter)
(make-charset-guard guard:lower-case?          char-set:lower-case)
(make-charset-guard guard:upper-case?          char-set:upper-case)
(make-charset-guard guard:digit?               char-set:digit)
(make-charset-guard guard:letter+digit?        char-set:letter+digit)
(make-charset-guard guard:graphic?             char-set:graphic)
(make-charset-guard guard:printing?            char-set:printing)
(make-charset-guard guard:whitespace?          char-set:whitespace)
(make-charset-guard guard:blank?               char-set:blank)
(make-charset-guard guard:punctuation?         char-set:punctuation)
(make-charset-guard guard:symbol?              char-set:symbol)
(make-charset-guard guard:hex-digit?           char-set:hex-digit)
(make-charset-guard guard:ascii?               char-set:ascii)

;; Misc. procedures.
(make-char-guard guard:newline?                #\newline)
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

;;; u8.scm -- Guile-SMC finite state machine u8 context.

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

;; This file contains an implementation of a binary context based on a
;; generic context. The binary context can be used to handle a stream of
;; bytes.


;;; Code:

(define-module (smc context u8)
  #:use-module (ice-9 binary-ports)
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
               guard:#t
               action:no-op
               action:store
               action:update-stanza)
  #:export (<u8-context>
            u8-context-port
            u8-context-counter
            u8-context-update-counters!

            u8-context-event-source

            ;; Actions.
            u8-context-syntax-error

            ;; All guards that are exported with 'define-public' below.

            ;; Logging procedures
            u8-context-log-error
            u8-context-log-warning
            u8-context-log-info
            u8-context-log-debug))


(define-class <u8-context> (<port-context>))


(define u8-context-port context-port)
(define u8-context-counter context-counter)

;; Update counters in a character context CTX based on an incoming character
;; CH.  These counters are thrown when a syntax error occurred.
(define-method (u8-context-update-counters! (ctx <u8-context>) byte)
  (unless (eof-object? byte)
    (context-counter++! ctx)))


;;; Event source.

;; Get the next character from a CONTEXT port.
(define-method (u8-context-event-source (context <u8-context>))
  (let ((byte (get-u8 (u8-context-port context))))
    (u8-context-update-counters! context byte)
    byte))


;;; Guards.

;; Make a procedure that checks if a CH1 equals to CH2.
(define-syntax-rule (make-char-guard name ch)
  (define-public (name ctx byte)
    (char=? (integer->char byte) ch)))

;; Make a procedure that checks if a CH is in a CHARSET.
(define-syntax-rule (make-charset-guard name charset)
  (define-public (name ctx byte)
    (char-set-contains? charset (integer->char byte))))

;;; The procedures below are predicates for symbols of the ASCII table, in the
;;; same order.

(make-char-guard u8:nul?                    #\nul)
(make-char-guard u8:soh?                    #\soh)
(make-char-guard u8:stx?                    #\stx)
(make-char-guard u8:etx?                    #\etx)
(make-char-guard u8:eot?                    #\eot)
(make-char-guard u8:enq?                    #\enq)
(make-char-guard u8:ack?                    #\ack)
(make-char-guard u8:bel?                    #\bel)
(make-char-guard u8:bs?                     #\bs)
(make-char-guard u8:tab?                    #\tab)
(make-char-guard u8:lf?                     #\linefeed)
(make-char-guard u8:vt?                     #\vtab)
(make-char-guard u8:ff?                     #\ff)
(make-char-guard u8:cr?                     #\cr)
(make-char-guard u8:so?                     #\so)
(make-char-guard u8:si?                     #\si)
(make-char-guard u8:dle?                    #\dle)
(make-char-guard u8:dc1?                    #\dc1)
(make-char-guard u8:dc2?                    #\dc2)
(make-char-guard u8:dc3?                    #\dc3)
(make-char-guard u8:dc4?                    #\dc4)
(make-char-guard u8:nak?                    #\nak)
(make-char-guard u8:syn?                    #\syn)
(make-char-guard u8:etb?                    #\etb)
(make-char-guard u8:can?                    #\can)
(make-char-guard u8:em?                     #\em)
(make-char-guard u8:sub?                    #\sub)
(make-char-guard u8:esc?                    #\esc)
(make-char-guard u8:fs?                     #\fs)
(make-char-guard u8:gs?                     #\gs)
(make-char-guard u8:rs?                     #\rs)
(make-char-guard u8:us?                     #\us)
(make-char-guard u8:space?                  #\space)
(make-char-guard u8:exclamation-mark?       #\!)
(make-char-guard u8:double-quote?           #\")
(make-char-guard u8:number-sign?            #\#)
(make-char-guard u8:dollar-sign?            #\$)
(make-char-guard u8:percent-sign?           #\%)
(make-char-guard u8:ampersand?              #\&)
(make-char-guard u8:single-quote?           #\')
(make-char-guard u8:left-parenthesis?       #\()
(make-char-guard u8:right-parenthesis?      #\))
(make-char-guard u8:asterisk?               #\*)
(make-char-guard u8:plus-sign?              #\+)
(make-char-guard u8:comma?                  #\,)
(make-char-guard u8:hyphen-minus?           #\-)
(make-char-guard u8:full-stop?              #\.)
(make-char-guard u8:solidus?                #\/)

;; BEGIN: Digits.
(make-char-guard u8:digit-zero?             #\0)
(make-char-guard u8:digit-one?              #\1)
(make-char-guard u8:digit-two?              #\2)
(make-char-guard u8:digit-three?            #\3)
(make-char-guard u8:digit-four?             #\4)
(make-char-guard u8:digit-five?             #\5)
(make-char-guard u8:digit-six?              #\6)
(make-char-guard u8:digit-seven?            #\7)
(make-char-guard u8:digit-eight?            #\8)
(make-char-guard u8:digit-nine?             #\9)
;; END: Digits.

(make-char-guard u8:colon?                  #\:)
(make-char-guard u8:semicolon?              #\;)
(make-char-guard u8:less-than-sign?         #\<)
(make-char-guard u8:equals-sign?            #\=)
(make-char-guard u8:more-than-sign?         #\>)
(make-char-guard u8:question-mark?          #\?)
(make-char-guard u8:at-symbol?              #\@)

;; BEGIN: Uppercase letters.
(make-char-guard u8:letter-A?               #\A)
(make-char-guard u8:letter-B?               #\B)
(make-char-guard u8:letter-C?               #\C)
(make-char-guard u8:letter-D?               #\D)
(make-char-guard u8:letter-E?               #\E)
(make-char-guard u8:letter-F?               #\F)
(make-char-guard u8:letter-G?               #\G)
(make-char-guard u8:letter-H?               #\H)
(make-char-guard u8:letter-I?               #\I)
(make-char-guard u8:letter-J?               #\J)
(make-char-guard u8:letter-K?               #\K)
(make-char-guard u8:letter-L?               #\L)
(make-char-guard u8:letter-M?               #\M)
(make-char-guard u8:letter-N?               #\N)
(make-char-guard u8:letter-O?               #\O)
(make-char-guard u8:letter-P?               #\P)
(make-char-guard u8:letter-Q?               #\Q)
(make-char-guard u8:letter-R?               #\R)
(make-char-guard u8:letter-S?               #\S)
(make-char-guard u8:letter-T?               #\T)
(make-char-guard u8:letter-U?               #\U)
(make-char-guard u8:letter-V?               #\V)
(make-char-guard u8:letter-W?               #\W)
(make-char-guard u8:letter-X?               #\X)
(make-char-guard u8:letter-Y?               #\Y)
(make-char-guard u8:letter-Z?               #\Z)
;; END: Uppercase letter.

(make-char-guard u8:left-square-bracket?    #\[)
(make-char-guard u8:reverse-solidus?        #\\)
(make-char-guard u8:right-square-bracket?   #\])
(make-char-guard u8:circumflex-accent?      #\^)
(make-char-guard u8:low-line?               #\_)
(make-char-guard u8:grave-accent?           #\`)

;; BEGIN: Lowercase letters.
(make-char-guard u8:letter-a?               #\a)
(make-char-guard u8:letter-b?               #\b)
(make-char-guard u8:letter-c?               #\c)
(make-char-guard u8:letter-d?               #\d)
(make-char-guard u8:letter-e?               #\e)
(make-char-guard u8:letter-f?               #\f)
(make-char-guard u8:letter-g?               #\g)
(make-char-guard u8:letter-h?               #\h)
(make-char-guard u8:letter-i?               #\i)
(make-char-guard u8:letter-j?               #\j)
(make-char-guard u8:letter-k?               #\k)
(make-char-guard u8:letter-l?               #\l)
(make-char-guard u8:letter-m?               #\m)
(make-char-guard u8:letter-n?               #\n)
(make-char-guard u8:letter-o?               #\o)
(make-char-guard u8:letter-p?               #\p)
(make-char-guard u8:letter-q?               #\q)
(make-char-guard u8:letter-r?               #\r)
(make-char-guard u8:letter-s?               #\s)
(make-char-guard u8:letter-t?               #\t)
(make-char-guard u8:letter-u?               #\u)
(make-char-guard u8:letter-v?               #\v)
(make-char-guard u8:letter-w?               #\w)
(make-char-guard u8:letter-x?               #\x)
(make-char-guard u8:letter-y?               #\y)
(make-char-guard u8:letter-z?               #\z)
;; END: Lowercase letters.

(make-char-guard u8:left-curly-bracket?     #\{)
(make-char-guard u8:vertical-line?          #\|)
(make-char-guard u8:right-curly-bracket?    #\})
(make-char-guard u8:tilde?                  #\~)
(make-char-guard u8:del?                    (integer->char 127))

;; Charset predicates.
(make-charset-guard u8:letter?              char-set:letter)
(make-charset-guard u8:lower-case?          char-set:lower-case)
(make-charset-guard u8:upper-case?          char-set:upper-case)
(make-charset-guard u8:digit?               char-set:digit)
(make-charset-guard u8:letter+digit?        char-set:letter+digit)
(make-charset-guard u8:graphic?             char-set:graphic)
(make-charset-guard u8:printing?            char-set:printing)
(make-charset-guard u8:whitespace?          char-set:whitespace)
(make-charset-guard u8:blank?               char-set:blank)
(make-charset-guard u8:punctuation?         char-set:punctuation)
(make-charset-guard u8:symbol?              char-set:symbol)
(make-charset-guard u8:hex-digit?           char-set:hex-digit)
(make-charset-guard u8:ascii?               char-set:ascii)

;; Misc. procedures.
(make-char-guard u8:newline?                #\newline)
(define-public (u8:eof-object? ctx ch)
  (eof-object? ch))



(define (u8-context-syntax-error ctx byte)
  (error "Syntax error"
         (u8-context-port ctx)
         (u8-context-counter ctx)
         byte
         ctx))



(define (%current-position-prefix ctx)
  (format #f "~a:~a: "
          (u8-context-port ctx)
          (u8-context-counter ctx)))

(define (u8-context-log-error ctx fmt . rest)
  (apply log-error
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (u8-context-log-warning ctx fmt . rest)
  (apply log-warning
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (u8-context-log-info ctx fmt . rest)
  (apply log-info
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (u8-context-log-debug ctx fmt . rest)
  (apply log-debug
         (string-append (%current-position-prefix ctx) fmt)
         rest))

;;; u8.scm ends here.

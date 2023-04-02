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

(define-module (smc context char)
  #:use-module (ice-9 textual-ports)
  #:use-module (oop goops)
  #:use-module (smc context common)
  #:re-export (guard:#t
               action:no-op)
  #:export (char:make-guard
            char:make-charset-guard))


;;; Guards.

;; Make a procedure that checks if a CH1 equals to CH2.
(define-syntax-rule (char:make-guard name ch1)
  (begin
    (define (name ctx ch2)
      (char=? ch1 ch2))
    (export name)))

;; Make a procedure that checks if a CH is in a CHARSET.
(define-syntax-rule (char:make-charset-guard name charset)
  (begin
    (define (name ctx ch)
      (char-set-contains? charset ch))
    (export name)))

;;; The procedures below are predicates for symbols of the ASCII table, in the
;;; same order.

(char:make-guard char:nul?                    #\nul)
(char:make-guard char:soh?                    #\soh)
(char:make-guard char:stx?                    #\stx)
(char:make-guard char:etx?                    #\etx)
(char:make-guard char:eot?                    #\eot)
(char:make-guard char:enq?                    #\enq)
(char:make-guard char:ack?                    #\ack)
(char:make-guard char:bel?                    #\bel)
(char:make-guard char:bs?                     #\bs)
(char:make-guard char:tab?                    #\tab)
(char:make-guard char:lf?                     #\linefeed)
(char:make-guard char:vt?                     #\vtab)
(char:make-guard char:ff?                     #\ff)
(char:make-guard char:cr?                     #\cr)
(char:make-guard char:so?                     #\so)
(char:make-guard char:si?                     #\si)
(char:make-guard char:dle?                    #\dle)
(char:make-guard char:dc1?                    #\dc1)
(char:make-guard char:dc2?                    #\dc2)
(char:make-guard char:dc3?                    #\dc3)
(char:make-guard char:dc4?                    #\dc4)
(char:make-guard char:nak?                    #\nak)
(char:make-guard char:syn?                    #\syn)
(char:make-guard char:etb?                    #\etb)
(char:make-guard char:can?                    #\can)
(char:make-guard char:em?                     #\em)
(char:make-guard char:sub?                    #\sub) ; Ctrl+Z
(char:make-guard char:esc?                    #\esc)
(char:make-guard char:fs?                     #\fs)
(char:make-guard char:gs?                     #\gs)
(char:make-guard char:rs?                     #\rs)
(char:make-guard char:us?                     #\us)
(char:make-guard char:space?                  #\space)
(char:make-guard char:exclamation-mark?       #\!)
(char:make-guard char:double-quote?           #\")
(char:make-guard char:number-sign?            #\#)
(char:make-guard char:dollar-sign?            #\$)
(char:make-guard char:percent-sign?           #\%)
(char:make-guard char:ampersand?              #\&)
(char:make-guard char:single-quote?           #\')
(char:make-guard char:left-parenthesis?       #\()
(char:make-guard char:right-parenthesis?      #\))
(char:make-guard char:asterisk?               #\*)
(char:make-guard char:plus-sign?              #\+)
(char:make-guard char:comma?                  #\,)
(char:make-guard char:hyphen-minus?           #\-)
(char:make-guard char:full-stop?              #\.)
(char:make-guard char:solidus?                #\/)

;; BEGIN: Digits.
(char:make-guard char:digit-zero?             #\0)
(char:make-guard char:digit-one?              #\1)
(char:make-guard char:digit-two?              #\2)
(char:make-guard char:digit-three?            #\3)
(char:make-guard char:digit-four?             #\4)
(char:make-guard char:digit-five?             #\5)
(char:make-guard char:digit-six?              #\6)
(char:make-guard char:digit-seven?            #\7)
(char:make-guard char:digit-eight?            #\8)
(char:make-guard char:digit-nine?             #\9)
;; END: Digits.

(char:make-guard char:colon?                  #\:)
(char:make-guard char:semicolon?              #\;)
(char:make-guard char:less-than-sign?         #\<)
(char:make-guard char:equals-sign?            #\=)
(char:make-guard char:more-than-sign?         #\>)
(char:make-guard char:question-mark?          #\?)
(char:make-guard char:at-symbol?              #\@)

;; BEGIN: Uppercase letters.
(char:make-guard char:letter-A?               #\A)
(char:make-guard char:letter-B?               #\B)
(char:make-guard char:letter-C?               #\C)
(char:make-guard char:letter-D?               #\D)
(char:make-guard char:letter-E?               #\E)
(char:make-guard char:letter-F?               #\F)
(char:make-guard char:letter-G?               #\G)
(char:make-guard char:letter-H?               #\H)
(char:make-guard char:letter-I?               #\I)
(char:make-guard char:letter-J?               #\J)
(char:make-guard char:letter-K?               #\K)
(char:make-guard char:letter-L?               #\L)
(char:make-guard char:letter-M?               #\M)
(char:make-guard char:letter-N?               #\N)
(char:make-guard char:letter-O?               #\O)
(char:make-guard char:letter-P?               #\P)
(char:make-guard char:letter-Q?               #\Q)
(char:make-guard char:letter-R?               #\R)
(char:make-guard char:letter-S?               #\S)
(char:make-guard char:letter-T?               #\T)
(char:make-guard char:letter-U?               #\U)
(char:make-guard char:letter-V?               #\V)
(char:make-guard char:letter-W?               #\W)
(char:make-guard char:letter-X?               #\X)
(char:make-guard char:letter-Y?               #\Y)
(char:make-guard char:letter-Z?               #\Z)
;; END: Uppercase letter.

(char:make-guard char:left-square-bracket?    #\[)
(char:make-guard char:reverse-solidus?        #\\)
(char:make-guard char:right-square-bracket?   #\])
(char:make-guard char:circumflex-accent?      #\^)
(char:make-guard char:low-line?               #\_)
(char:make-guard char:grave-accent?           #\`)

;; BEGIN: Lowercase letters.
(char:make-guard char:letter-a?               #\a)
(char:make-guard char:letter-b?               #\b)
(char:make-guard char:letter-c?               #\c)
(char:make-guard char:letter-d?               #\d)
(char:make-guard char:letter-e?               #\e)
(char:make-guard char:letter-f?               #\f)
(char:make-guard char:letter-g?               #\g)
(char:make-guard char:letter-h?               #\h)
(char:make-guard char:letter-i?               #\i)
(char:make-guard char:letter-j?               #\j)
(char:make-guard char:letter-k?               #\k)
(char:make-guard char:letter-l?               #\l)
(char:make-guard char:letter-m?               #\m)
(char:make-guard char:letter-n?               #\n)
(char:make-guard char:letter-o?               #\o)
(char:make-guard char:letter-p?               #\p)
(char:make-guard char:letter-q?               #\q)
(char:make-guard char:letter-r?               #\r)
(char:make-guard char:letter-s?               #\s)
(char:make-guard char:letter-t?               #\t)
(char:make-guard char:letter-u?               #\u)
(char:make-guard char:letter-v?               #\v)
(char:make-guard char:letter-w?               #\w)
(char:make-guard char:letter-x?               #\x)
(char:make-guard char:letter-y?               #\y)
(char:make-guard char:letter-z?               #\z)
;; END: Lowercase letters.

(char:make-guard char:left-curly-bracket?     #\{)
(char:make-guard char:vertical-line?          #\|)
(char:make-guard char:right-curly-bracket?    #\})
(char:make-guard char:tilde?                  #\~)
(char:make-guard char:del?                    (integer->char 127))

;; Charset predicates.
(char:make-charset-guard char:letter?              char-set:letter)
(char:make-charset-guard char:lower-case?          char-set:lower-case)
(char:make-charset-guard char:upper-case?          char-set:upper-case)
(char:make-charset-guard char:digit?               char-set:digit)
(char:make-charset-guard char:letter+digit?        char-set:letter+digit)
(char:make-charset-guard char:graphic?             char-set:graphic)
(char:make-charset-guard char:printing?            char-set:printing)
(char:make-charset-guard char:whitespace?          char-set:whitespace)
(char:make-charset-guard char:blank?               char-set:blank)
(char:make-charset-guard char:punctuation?         char-set:punctuation)
(char:make-charset-guard char:symbol?              char-set:symbol)
(char:make-charset-guard char:hex-digit?           char-set:hex-digit)
(char:make-charset-guard char:ascii?               char-set:ascii)

;; Misc. procedures.
(char:make-guard char:newline?                #\newline)
(define-public (char:eof-object? ctx ch)
  (eof-object? ch))

;;; char.scm ends here.

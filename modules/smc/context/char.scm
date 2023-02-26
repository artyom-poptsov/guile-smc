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
  #:export (make-char-guard
            make-charset-guard))


;;; Guards.

;; Make a procedure that checks if a CH1 equals to CH2.
(define-syntax-rule (make-char-guard name ch1)
  (begin
    (define-method (name (ctx <top>) (ch2 <char>))
      (char=? ch1 ch2))
    (export name)))

;; Make a procedure that checks if a CH is in a CHARSET.
(define-syntax-rule (make-charset-guard name charset)
  (begin
    (define-method (name (ctx <top>) (ch <char>))
      (char-set-contains? charset ch))
    (export name)))

;;; The procedures below are predicates for symbols of the ASCII table, in the
;;; same order.

(make-char-guard char:nul?                    #\nul)
(make-char-guard char:soh?                    #\soh)
(make-char-guard char:stx?                    #\stx)
(make-char-guard char:etx?                    #\etx)
(make-char-guard char:eot?                    #\eot)
(make-char-guard char:enq?                    #\enq)
(make-char-guard char:ack?                    #\ack)
(make-char-guard char:bel?                    #\bel)
(make-char-guard char:bs?                     #\bs)
(make-char-guard char:tab?                    #\tab)
(make-char-guard char:lf?                     #\linefeed)
(make-char-guard char:vt?                     #\vtab)
(make-char-guard char:ff?                     #\ff)
(make-char-guard char:cr?                     #\cr)
(make-char-guard char:so?                     #\so)
(make-char-guard char:si?                     #\si)
(make-char-guard char:dle?                    #\dle)
(make-char-guard char:dc1?                    #\dc1)
(make-char-guard char:dc2?                    #\dc2)
(make-char-guard char:dc3?                    #\dc3)
(make-char-guard char:dc4?                    #\dc4)
(make-char-guard char:nak?                    #\nak)
(make-char-guard char:syn?                    #\syn)
(make-char-guard char:etb?                    #\etb)
(make-char-guard char:can?                    #\can)
(make-char-guard char:em?                     #\em)
(make-char-guard char:sub?                    #\sub)
(make-char-guard char:esc?                    #\esc)
(make-char-guard char:fs?                     #\fs)
(make-char-guard char:gs?                     #\gs)
(make-char-guard char:rs?                     #\rs)
(make-char-guard char:us?                     #\us)
(make-char-guard char:space?                  #\space)
(make-char-guard char:exclamation-mark?       #\!)
(make-char-guard char:double-quote?           #\")
(make-char-guard char:number-sign?            #\#)
(make-char-guard char:dollar-sign?            #\$)
(make-char-guard char:percent-sign?           #\%)
(make-char-guard char:ampersand?              #\&)
(make-char-guard char:single-quote?           #\')
(make-char-guard char:left-parenthesis?       #\()
(make-char-guard char:right-parenthesis?      #\))
(make-char-guard char:asterisk?               #\*)
(make-char-guard char:plus-sign?              #\+)
(make-char-guard char:comma?                  #\,)
(make-char-guard char:hyphen-minus?           #\-)
(make-char-guard char:full-stop?              #\.)
(make-char-guard char:solidus?                #\/)

;; BEGIN: Digits.
(make-char-guard char:digit-zero?             #\0)
(make-char-guard char:digit-one?              #\1)
(make-char-guard char:digit-two?              #\2)
(make-char-guard char:digit-three?            #\3)
(make-char-guard char:digit-four?             #\4)
(make-char-guard char:digit-five?             #\5)
(make-char-guard char:digit-six?              #\6)
(make-char-guard char:digit-seven?            #\7)
(make-char-guard char:digit-eight?            #\8)
(make-char-guard char:digit-nine?             #\9)
;; END: Digits.

(make-char-guard char:colon?                  #\:)
(make-char-guard char:semicolon?              #\;)
(make-char-guard char:less-than-sign?         #\<)
(make-char-guard char:equals-sign?            #\=)
(make-char-guard char:more-than-sign?         #\>)
(make-char-guard char:question-mark?          #\?)
(make-char-guard char:at-symbol?              #\@)

;; BEGIN: Uppercase letters.
(make-char-guard char:letter-A?               #\A)
(make-char-guard char:letter-B?               #\B)
(make-char-guard char:letter-C?               #\C)
(make-char-guard char:letter-D?               #\D)
(make-char-guard char:letter-E?               #\E)
(make-char-guard char:letter-F?               #\F)
(make-char-guard char:letter-G?               #\G)
(make-char-guard char:letter-H?               #\H)
(make-char-guard char:letter-I?               #\I)
(make-char-guard char:letter-J?               #\J)
(make-char-guard char:letter-K?               #\K)
(make-char-guard char:letter-L?               #\L)
(make-char-guard char:letter-M?               #\M)
(make-char-guard char:letter-N?               #\N)
(make-char-guard char:letter-O?               #\O)
(make-char-guard char:letter-P?               #\P)
(make-char-guard char:letter-Q?               #\Q)
(make-char-guard char:letter-R?               #\R)
(make-char-guard char:letter-S?               #\S)
(make-char-guard char:letter-T?               #\T)
(make-char-guard char:letter-U?               #\U)
(make-char-guard char:letter-V?               #\V)
(make-char-guard char:letter-W?               #\W)
(make-char-guard char:letter-X?               #\X)
(make-char-guard char:letter-Y?               #\Y)
(make-char-guard char:letter-Z?               #\Z)
;; END: Uppercase letter.

(make-char-guard char:left-square-bracket?    #\[)
(make-char-guard char:reverse-solidus?        #\\)
(make-char-guard char:right-square-bracket?   #\])
(make-char-guard char:circumflex-accent?      #\^)
(make-char-guard char:low-line?               #\_)
(make-char-guard char:grave-accent?           #\`)

;; BEGIN: Lowercase letters.
(make-char-guard char:letter-a?               #\a)
(make-char-guard char:letter-b?               #\b)
(make-char-guard char:letter-c?               #\c)
(make-char-guard char:letter-d?               #\d)
(make-char-guard char:letter-e?               #\e)
(make-char-guard char:letter-f?               #\f)
(make-char-guard char:letter-g?               #\g)
(make-char-guard char:letter-h?               #\h)
(make-char-guard char:letter-i?               #\i)
(make-char-guard char:letter-j?               #\j)
(make-char-guard char:letter-k?               #\k)
(make-char-guard char:letter-l?               #\l)
(make-char-guard char:letter-m?               #\m)
(make-char-guard char:letter-n?               #\n)
(make-char-guard char:letter-o?               #\o)
(make-char-guard char:letter-p?               #\p)
(make-char-guard char:letter-q?               #\q)
(make-char-guard char:letter-r?               #\r)
(make-char-guard char:letter-s?               #\s)
(make-char-guard char:letter-t?               #\t)
(make-char-guard char:letter-u?               #\u)
(make-char-guard char:letter-v?               #\v)
(make-char-guard char:letter-w?               #\w)
(make-char-guard char:letter-x?               #\x)
(make-char-guard char:letter-y?               #\y)
(make-char-guard char:letter-z?               #\z)
;; END: Lowercase letters.

(make-char-guard char:left-curly-bracket?     #\{)
(make-char-guard char:vertical-line?          #\|)
(make-char-guard char:right-curly-bracket?    #\})
(make-char-guard char:tilde?                  #\~)
(make-char-guard char:del?                    (integer->char 127))

;; Charset predicates.
(make-charset-guard char:letter?              char-set:letter)
(make-charset-guard char:lower-case?          char-set:lower-case)
(make-charset-guard char:upper-case?          char-set:upper-case)
(make-charset-guard char:digit?               char-set:digit)
(make-charset-guard char:letter+digit?        char-set:letter+digit)
(make-charset-guard char:graphic?             char-set:graphic)
(make-charset-guard char:printing?            char-set:printing)
(make-charset-guard char:whitespace?          char-set:whitespace)
(make-charset-guard char:blank?               char-set:blank)
(make-charset-guard char:punctuation?         char-set:punctuation)
(make-charset-guard char:symbol?              char-set:symbol)
(make-charset-guard char:hex-digit?           char-set:hex-digit)
(make-charset-guard char:ascii?               char-set:ascii)

;; Misc. procedures.
(make-char-guard char:newline?                #\newline)
(define-public (char:eof-object? ctx ch)
  (eof-object? ch))

;;; char.scm ends here.

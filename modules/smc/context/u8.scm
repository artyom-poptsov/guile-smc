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

;; This file contains an implementation of a u8 guards.


;;; Code:

(define-module (smc context u8)
  #:use-module (smc context common)
  #:re-export (guard:#t
               action:no-op))


;; Make a procedure that checks if a CH1 equals to CH2.
(define-syntax-rule (u8:make-char-guard name ch)
  (define-public (name ctx byte)
    (char=? (integer->char byte) ch)))

;; Make a procedure that checks if a CH is in a CHARSET.
(define-syntax-rule (u8:make-charset-guard name charset)
  (define-public (name ctx byte)
    (char-set-contains? charset (integer->char byte))))

;;; The procedures below are predicates for symbols of the ASCII table, in the
;;; same order.

(u8:make-char-guard u8:nul?                    #\nul)
(u8:make-char-guard u8:soh?                    #\soh)
(u8:make-char-guard u8:stx?                    #\stx)
(u8:make-char-guard u8:etx?                    #\etx)
(u8:make-char-guard u8:eot?                    #\eot)
(u8:make-char-guard u8:enq?                    #\enq)
(u8:make-char-guard u8:ack?                    #\ack)
(u8:make-char-guard u8:bel?                    #\bel)
(u8:make-char-guard u8:bs?                     #\bs)
(u8:make-char-guard u8:tab?                    #\tab)
(u8:make-char-guard u8:lf?                     #\linefeed)
(u8:make-char-guard u8:vt?                     #\vtab)
(u8:make-char-guard u8:ff?                     #\ff)
(u8:make-char-guard u8:cr?                     #\cr)
(u8:make-char-guard u8:so?                     #\so)
(u8:make-char-guard u8:si?                     #\si)
(u8:make-char-guard u8:dle?                    #\dle)
(u8:make-char-guard u8:dc1?                    #\dc1)
(u8:make-char-guard u8:dc2?                    #\dc2)
(u8:make-char-guard u8:dc3?                    #\dc3)
(u8:make-char-guard u8:dc4?                    #\dc4)
(u8:make-char-guard u8:nak?                    #\nak)
(u8:make-char-guard u8:syn?                    #\syn)
(u8:make-char-guard u8:etb?                    #\etb)
(u8:make-char-guard u8:can?                    #\can)
(u8:make-char-guard u8:em?                     #\em)
(u8:make-char-guard u8:sub?                    #\sub)
(u8:make-char-guard u8:esc?                    #\esc)
(u8:make-char-guard u8:fs?                     #\fs)
(u8:make-char-guard u8:gs?                     #\gs)
(u8:make-char-guard u8:rs?                     #\rs)
(u8:make-char-guard u8:us?                     #\us)
(u8:make-char-guard u8:space?                  #\space)
(u8:make-char-guard u8:exclamation-mark?       #\!)
(u8:make-char-guard u8:double-quote?           #\")
(u8:make-char-guard u8:number-sign?            #\#)
(u8:make-char-guard u8:dollar-sign?            #\$)
(u8:make-char-guard u8:percent-sign?           #\%)
(u8:make-char-guard u8:ampersand?              #\&)
(u8:make-char-guard u8:single-quote?           #\')
(u8:make-char-guard u8:left-parenthesis?       #\()
(u8:make-char-guard u8:right-parenthesis?      #\))
(u8:make-char-guard u8:asterisk?               #\*)
(u8:make-char-guard u8:plus-sign?              #\+)
(u8:make-char-guard u8:comma?                  #\,)
(u8:make-char-guard u8:hyphen-minus?           #\-)
(u8:make-char-guard u8:full-stop?              #\.)
(u8:make-char-guard u8:solidus?                #\/)

;; BEGIN: Digits.
(u8:make-char-guard u8:digit-zero?             #\0)
(u8:make-char-guard u8:digit-one?              #\1)
(u8:make-char-guard u8:digit-two?              #\2)
(u8:make-char-guard u8:digit-three?            #\3)
(u8:make-char-guard u8:digit-four?             #\4)
(u8:make-char-guard u8:digit-five?             #\5)
(u8:make-char-guard u8:digit-six?              #\6)
(u8:make-char-guard u8:digit-seven?            #\7)
(u8:make-char-guard u8:digit-eight?            #\8)
(u8:make-char-guard u8:digit-nine?             #\9)
;; END: Digits.

(u8:make-char-guard u8:colon?                  #\:)
(u8:make-char-guard u8:semicolon?              #\;)
(u8:make-char-guard u8:less-than-sign?         #\<)
(u8:make-char-guard u8:equals-sign?            #\=)
(u8:make-char-guard u8:more-than-sign?         #\>)
(u8:make-char-guard u8:question-mark?          #\?)
(u8:make-char-guard u8:at-symbol?              #\@)

;; BEGIN: Uppercase letters.
(u8:make-char-guard u8:letter-A?               #\A)
(u8:make-char-guard u8:letter-B?               #\B)
(u8:make-char-guard u8:letter-C?               #\C)
(u8:make-char-guard u8:letter-D?               #\D)
(u8:make-char-guard u8:letter-E?               #\E)
(u8:make-char-guard u8:letter-F?               #\F)
(u8:make-char-guard u8:letter-G?               #\G)
(u8:make-char-guard u8:letter-H?               #\H)
(u8:make-char-guard u8:letter-I?               #\I)
(u8:make-char-guard u8:letter-J?               #\J)
(u8:make-char-guard u8:letter-K?               #\K)
(u8:make-char-guard u8:letter-L?               #\L)
(u8:make-char-guard u8:letter-M?               #\M)
(u8:make-char-guard u8:letter-N?               #\N)
(u8:make-char-guard u8:letter-O?               #\O)
(u8:make-char-guard u8:letter-P?               #\P)
(u8:make-char-guard u8:letter-Q?               #\Q)
(u8:make-char-guard u8:letter-R?               #\R)
(u8:make-char-guard u8:letter-S?               #\S)
(u8:make-char-guard u8:letter-T?               #\T)
(u8:make-char-guard u8:letter-U?               #\U)
(u8:make-char-guard u8:letter-V?               #\V)
(u8:make-char-guard u8:letter-W?               #\W)
(u8:make-char-guard u8:letter-X?               #\X)
(u8:make-char-guard u8:letter-Y?               #\Y)
(u8:make-char-guard u8:letter-Z?               #\Z)
;; END: Uppercase letter.

(u8:make-char-guard u8:left-square-bracket?    #\[)
(u8:make-char-guard u8:reverse-solidus?        #\\)
(u8:make-char-guard u8:right-square-bracket?   #\])
(u8:make-char-guard u8:circumflex-accent?      #\^)
(u8:make-char-guard u8:low-line?               #\_)
(u8:make-char-guard u8:grave-accent?           #\`)

;; BEGIN: Lowercase letters.
(u8:make-char-guard u8:letter-a?               #\a)
(u8:make-char-guard u8:letter-b?               #\b)
(u8:make-char-guard u8:letter-c?               #\c)
(u8:make-char-guard u8:letter-d?               #\d)
(u8:make-char-guard u8:letter-e?               #\e)
(u8:make-char-guard u8:letter-f?               #\f)
(u8:make-char-guard u8:letter-g?               #\g)
(u8:make-char-guard u8:letter-h?               #\h)
(u8:make-char-guard u8:letter-i?               #\i)
(u8:make-char-guard u8:letter-j?               #\j)
(u8:make-char-guard u8:letter-k?               #\k)
(u8:make-char-guard u8:letter-l?               #\l)
(u8:make-char-guard u8:letter-m?               #\m)
(u8:make-char-guard u8:letter-n?               #\n)
(u8:make-char-guard u8:letter-o?               #\o)
(u8:make-char-guard u8:letter-p?               #\p)
(u8:make-char-guard u8:letter-q?               #\q)
(u8:make-char-guard u8:letter-r?               #\r)
(u8:make-char-guard u8:letter-s?               #\s)
(u8:make-char-guard u8:letter-t?               #\t)
(u8:make-char-guard u8:letter-u?               #\u)
(u8:make-char-guard u8:letter-v?               #\v)
(u8:make-char-guard u8:letter-w?               #\w)
(u8:make-char-guard u8:letter-x?               #\x)
(u8:make-char-guard u8:letter-y?               #\y)
(u8:make-char-guard u8:letter-z?               #\z)
;; END: Lowercase letters.

(u8:make-char-guard u8:left-curly-bracket?     #\{)
(u8:make-char-guard u8:vertical-line?          #\|)
(u8:make-char-guard u8:right-curly-bracket?    #\})
(u8:make-char-guard u8:tilde?                  #\~)
(u8:make-char-guard u8:del?                    (integer->char 127))

;; Charset predicates.
(u8:make-charset-guard u8:letter?              char-set:letter)
(u8:make-charset-guard u8:lower-case?          char-set:lower-case)
(u8:make-charset-guard u8:upper-case?          char-set:upper-case)
(u8:make-charset-guard u8:digit?               char-set:digit)
(u8:make-charset-guard u8:letter+digit?        char-set:letter+digit)
(u8:make-charset-guard u8:graphic?             char-set:graphic)
(u8:make-charset-guard u8:printing?            char-set:printing)
(u8:make-charset-guard u8:whitespace?          char-set:whitespace)
(u8:make-charset-guard u8:blank?               char-set:blank)
(u8:make-charset-guard u8:punctuation?         char-set:punctuation)
(u8:make-charset-guard u8:symbol?              char-set:symbol)
(u8:make-charset-guard u8:hex-digit?           char-set:hex-digit)
(u8:make-charset-guard u8:ascii?               char-set:ascii)

;; Misc. procedures.
(u8:make-char-guard u8:newline?                #\newline)
(define-public (u8:eof-object? ctx ch)
  (eof-object? ch))

;;; u8.scm ends here.

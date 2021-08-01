;;; stack.scm -- A stack data structure implementation.

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

;; A naive stack implementation in Scheme with dreaded side-effect-laden
;; procedures.


;;; Code:

(define-module (smc core stack)
  #:use-module (oop goops)
  #:use-module (smc core common)
  #:export (<stack>
            stack?
            stack-content
            stack-content/reversed
            stack-push!
            stack-pop!
            stack-clear!
            stack-size
            stack-empty?))



(define-class <stack> ()
  ;; <list>
  (content
   #:init-value '()
   #:getter     stack-content
   #:setter     stack-content-set!))

(define (stack? x)
  (is-a? x <stack>))


(define-method (%display (stack <stack>) (port <port>))
  (format port "#<stack depth: ~a ~a>"
          (stack-size stack)
          (object-address/hex-string stack)))

(define-method (display (stack <stack>) (port <port>))
  (%display stack port))

(define-method (write (stack <stack>) (port <port>))
  (%display stack port))



;; Push an ELEMENT to a STACK.
(define-method (stack-push! (stack <stack>) element)
  (stack-content-set! stack (cons element (stack-content stack))))

;; Pop an element from a STACK.
(define-method (stack-pop! (stack <stack>))
  (let ((element (car (stack-content stack))))
    (stack-content-set! stack (cdr (stack-content stack)))
    element))

(define-method (stack-clear! (stack <stack>))
  (stack-content-set! stack '()))

;; Get the stack depth (size.)
(define-method (stack-size (stack <stack>))
  (length (stack-content stack)))

(define-method (stack-empty? (stack <stack>))
  (null? (stack-content stack)))

;; Get the content of the stack in reversed order.
(define-method (stack-content/reversed (stack <stack>))
  (reverse (stack-content stack)))

;;; stack.scm ends here.

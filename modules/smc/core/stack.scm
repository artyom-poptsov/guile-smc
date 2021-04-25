;; A naive stack implementation in Scheme with dreaded side-effect-laden
;; procedures.

(define-module (smc core stack)
  #:use-module (oop goops)
  #:export (<stack>
            stack-content
            stack-content/reversed
            stack-push!
            stack-pop!
            stack-size))



(define-class <stack> ()
  ;; <list>
  (content
   #:init-value '()
   #:getter     stack-content
   #:setter     stack-content-set!))

;; Push an ELEMENT to a STACK.
(define-method (stack-push! (stack <stack>) element)
  (stack-content-set! stack (cons element (stack-content stack))))

;; Pop an element from a STACK.
(define-method (stack-pop! (stack <stack>))
  (let ((element (car (stack-content stack))))
    (stack-content-set! stack (cdr (stack-content stack)))
    element))

;; Get the stack depth (size.)
(define-method (stack-size (stack <stack>))
  (length (stack-content stack)))

;; Get the content of the stack in reversed order.
(define-method (stack-content/reversed (stack <stack>))
  (reverse (stack-content stack)))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (smc core stack))


(test-begin "stack")

(test-assert "stack?"
  (and (stack? (make <stack>))
       (not (stack? "not a stack"))))

(test-equal "stack-size"
  0
  (stack-size (make <stack>)))

(test-assert "stack-empty?"
  (stack-empty? (make <stack>)))

(test-equal "stack-push!"
  2
  (let ((stack (make <stack>)))
    (stack-push! stack "hello")
    (stack-push! stack "world")
    (stack-size stack)))

(test-equal "stack-pop!"
  "world"
  (let ((stack (make <stack>)))
    (stack-push! stack "hello")
    (stack-push! stack "world")
    (stack-pop! stack)))

(test-assert "stack-clear!"
  (let ((stack (make <stack>)))
    (stack-push! stack "hello")
    (stack-push! stack "world")
    (stack-clear! stack)
    (stack-empty? stack)))

(test-equal "stack-content"
  '("world" "hello")
  (let ((stack (make <stack>)))
    (stack-push! stack "hello")
    (stack-push! stack "world")
    (stack-content stack)))

(test-equal "stack-content/reversed"
  '("hello" "world")
  (let ((stack (make <stack>)))
    (stack-push! stack "hello")
    (stack-push! stack "world")
    (stack-content/reversed stack)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "stack")

(exit exit-status)

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (smc guards char)
             (smc fsm)
             (smc core state))


(test-begin "state")

(test-assert "state?"
  (and (state? (make <state> #:name 'state-1))
       (not (state? 'not-a-state))))

(test-equal "equal?"
  (make <state> #:name 'state-1)
  (make <state> #:name 'state-1))

(test-assert "state-transition-count"
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-2)))))
    (-  (state-transition-count state) 1)))

(test-assert "state-transition-add!"
  (let ((state (make <state> #:name 'state-1)))
    (state-transition-add! state
                           (const #t)
                           (const #t)
                           'state-2)
    (= (state-transition-count state) 1)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "state")

(exit exit-status)


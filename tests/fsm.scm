(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (oop goops)
             (smc guards char)
             (smc fsm)
             (smc core state))


(test-begin "fsm")

(test-equal "fsm-state-count"
  2
  (let ((fsm (make <fsm>
               #:transition-table `((state-1
                                     (guard:#t ,action:no-op state-2))
                                    (state-2
                                     (guard:#t ,action:no-op state-1))))))
    (fsm-state-count fsm)))


(test-equal "fsm-transition-count"
  2
  (let ((fsm (make <fsm>
               #:transition-table `((state-1
                                     (guard:#t ,action:no-op state-2))
                                    (state-2
                                     (guard:#t ,action:no-op state-1))))))
    (fsm-transition-count fsm)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "fsm")

(exit exit-status)

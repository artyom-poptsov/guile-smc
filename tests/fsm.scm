(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (oop goops)
             (smc context char-context)
             (smc fsm)
             (smc core state))


(test-begin "fsm")

(test-equal "a state in the transition table with description"
  "This is a description."
  (let ((fsm (make <fsm>
               #:transition-table `(((name        . state-1)
                                     (description . "This is a description.")
                                     (transitions
                                      (guard:#t ,action:no-op state-1)))))))
    (state-description (fsm-state fsm 'state-1))))

(test-equal "fsm-state-count"
  2
  (let ((fsm (make <fsm>
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (guard:#t ,action:no-op state-2)))
                                    ((name . state-2)
                                     (transitions
                                      (guard:#t ,action:no-op state-1)))))))
    (fsm-state-count fsm)))

(test-equal "fsm-transition-count"
  2
  (let ((fsm (make <fsm>
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (guard:#t ,action:no-op state-2)))
                                    ((name . state-2)
                                     (transitions
                                      (guard:#t ,action:no-op state-1)))))))
    (fsm-transition-count fsm)))

(test-equal "fsm-incoming-transition-count: without recurrent links"
  2
  (let ((fsm (make <fsm>
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (guard:#t ,action:no-op state-2)
                                      (guard:#t ,action:no-op state-1)))
                                    ((name . state-2)
                                     (transitions
                                      (guard:#t ,action:no-op state-1)
                                      (guard:#t ,action:no-op state-3)))
                                    ((name . state-3)
                                     (transitions
                                      (guard:#t ,action:no-op state-1)))))))
    (fsm-incoming-transition-count fsm (fsm-state fsm 'state-1))))

(test-equal "fsm-incoming-transition-count: with recurrent links"
  3
  (let ((fsm (make <fsm>
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (,guard:#t ,action:no-op state-2)
                                      (,guard:#t ,action:no-op state-1)))
                                    ((name . state-2)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)
                                      (,guard:#t ,action:no-op state-3)))
                                    ((name . state-3)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)))))))
    (fsm-incoming-transition-count fsm (fsm-state fsm 'state-1)
                                   #:include-recurrent-links? #t)))

(test-assert "fsm-state-reachable?: #t"
  (let ((fsm (make <fsm>
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (,guard:#t ,action:no-op state-2)
                                      (,guard:#t ,action:no-op state-1)))
                                    ((name . state-2)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)
                                      (,guard:#t ,action:no-op state-3)))
                                    ((name . state-3)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)))))))
    (fsm-state-reachable? fsm (fsm-state fsm 'state-3))))

(test-assert "fsm-state-reachable?: #f"
  (let ((fsm (make <fsm>
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)))
                                    ((name . state-2)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)
                                      (,guard:#t ,action:no-op state-3)))
                                    ((name . state-3)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)))))))
    (not (fsm-state-reachable? fsm (fsm-state fsm 'state-2)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "fsm")

(exit exit-status)

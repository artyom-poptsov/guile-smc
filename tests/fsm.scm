(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (smc context char-context)
             (tests test-context)
             (smc fsm)
             (smc core state))


(test-begin "fsm")



(test-assert "anonymous-procedure?: #t"
  (anonymous-procedure? (lambda () #t)))

(test-assert "anonymous-procedure?: #f"
  (not (anonymous-procedure? display)))

(test-assert "anonymous-procedure?: #f (not a procedure)"
  (not (anonymous-procedure? 'not-a-procedure)))



(test-assert "display"
  (let ((fsm (make <fsm>
               #:transition-table `(((name        . state-1)
                                     (description . "This is a description.")
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)))))))
    (display fsm (current-error-port))
    (newline (current-error-port))
    (string-match "#<fsm current-state: state-1 statistics: 0/0 [0-9a-z]+>"
                  (with-output-to-string
                    (lambda ()
                      (display fsm))))))

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


;;; fsm-state-add!

(test-assert "fsm-state-add!: Empty FSM"
  (let ((fsm (make <fsm>)))
    (fsm-state-add! fsm (make <state> #:name 'state-1))
    (fsm-state fsm 'state-1)))


;;; fsm-procedures

(test-assert "fsm-procedures"
  (let ((fsm (make <fsm>
               #:event-source     test-event-source
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
    (fsm-procedures fsm)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "fsm")

(exit exit-status)

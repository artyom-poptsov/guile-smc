(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
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

(test-equal "state-transition-count"
  2
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-1)
                                 (,guard:#t ,action:no-op state-2)))))
    (state-transition-count state)))

(test-equal "state-transition-count: to"
  1
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-1)
                                 (,guard:#t ,action:no-op state-2)))))
    (state-transition-count state 'state-2)))

(test-equal "state-transition-count/foreign"
  1
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-1)
                                 (,guard:#t ,action:no-op state-2)))))
    (state-transition-count/foreign state)))

(test-equal "state-recurrent-links-count"
  2
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-1)
                                 (,guard:#t ,action:no-op state-1)
                                 (,guard:#t ,action:no-op state-2)))))
    (state-recurrent-links-count state)))

(test-assert "state-has-recurrent-links?: #t"
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-1)
                                 (,guard:#t ,action:no-op state-2)))))
    (state-has-recurrent-links? state)))

(test-assert "state-has-recurrent-links?: #f"
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-2)
                                 (,guard:#t ,action:no-op state-3)))))
    (not (state-has-recurrent-links? state))))

(test-assert "state-has-final-transitions?: #t"
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-2)
                                 (,guard:#t ,action:no-op ,#f)))))
    (state-has-final-transitions? state)))

(test-assert "state-has-final-transitions?: #f"
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-2)
                                 (,guard:#t ,action:no-op state-3)))))
    (not (state-has-final-transitions? state))))

(test-equal "state-transition-add!"
  1
  (let ((state (make <state> #:name 'state-1)))
    (state-transition-add! state
                           (const #t)
                           (const #t)
                           'state-2)
    (state-transition-count state)))

(test-assert "state-dead-end?: #t"
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-1)))))
    (state-dead-end? state)))

(test-assert "state-run: no transitions"
  (let ((state (make <state> #:name 'state-1)))
    (receive (next-state context)
        (state-run state 'event '())
      (and (not next-state)
           (null? context)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "state")

(exit exit-status)


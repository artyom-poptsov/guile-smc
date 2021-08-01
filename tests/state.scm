(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (smc context char-context)
             (smc fsm)
             (smc core state))


(test-begin "state")


;;; Accessors tests.

(test-equal "state-transition:guard"
  guard:#t
  (state-transition:guard `(,guard:#t ,action:no-op state-1)))

(test-equal "state-transition:action"
  action:no-op
  (state-transition:action `(,guard:#t ,action:no-op state-1)))

(test-equal "state-transition:next-state"
  'state-1
  (state-transition:next-state `(,guard:#t ,action:no-op state-1)))

(define (event-source:example ctx)
  #t)

(define %state `((name         . state-name)
                 (description  . "State description")
                 (event-source . ,event-source:example)
                 (transitions
                  (,guard:#t ,action:no-op state-1))))

(test-equal "state:name"
  'state-name
  (state:name %state))

(test-equal "state:description"
  "State description"
  (state:description %state))

(test-equal "state:event-source"
  event-source:example
  (state:event-source %state))

(test-equal "state:event-source/name"
  'event-source:example
  (state:event-source/name %state))

(test-equal "state:transitions"
  `((,guard:#t ,action:no-op state-1))
  (state:transitions %state))


(test-assert "state?"
  (and (state? (make <state> #:name 'state-1))
       (not (state? 'not-a-state))))

(test-equal "equal?"
  (make <state> #:name 'state-1)
  (make <state> #:name 'state-1))

(test-assert "display"
  (let ((state (make <state> #:name 'state-1)))
    (string-match "#<state state-1 [0-9a-z]+>"
                  (with-output-to-string
                    (lambda ()
                      (display state))))))

(test-assert "write"
  (let ((state (make <state> #:name 'state-1)))
    (string-match "#<state state-1 [0-9a-z]+>"
                  (with-output-to-string
                    (lambda ()
                      (write state))))))

(test-equal "state-transition-add!"
  `((,guard:#t ,action:no-op state-1)
    (,guard:#t ,action:no-op state-2))
  (let ((state (make <state>
                 #:transitions `((,guard:#t ,action:no-op state-1)))))
    (state-transition-add! state guard:#t action:no-op 'state-2)
    (state-transitions state)))

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

(test-assert "state-dead-end: #f"
  (let ((state (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-1)
                                 (,guard:#t ,action:no-op state-2)))))
    (not (state-dead-end? state))))

(test-assert "state-run: no transitions"
  (let ((state (make <state> #:name 'state-1)))
    (receive (next-state context)
        (state-run state 'event '())
      (and (not next-state)
           (null? context)))))


;;; list->state

(test-equal "list->state"
  (make <state>
    #:name         'state-name
    #:description  "State description"
    #:event-source event-source:example
    #:transitions  `(,guard:#t ,action:no-op state-1))
  (list->state %state))


;;; state->list

(test-equal "state->list: W/o description and transitions"
  `((name         . state-1)
    (description  . #f)
    (event-source . #f)
    (transitions))
  (state->list (make <state> #:name 'state-1)))

(test-equal "state->list: With description"
  `((name         . state-1)
    (description  . "This is a description")
    (event-source . #f)
    (transitions))
  (state->list (make <state>
                 #:name 'state-1
                 #:description "This is a description")))

(test-equal "state->list: With transitions"
  `((name         . state-1)
    (description  . #f)
    (event-source . #f)
    (transitions
     (,guard:#t ,action:no-op state-2)))
  (state->list (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-2)))))

(test-equal "state->list: With description and transitions"
  `((name         . state-1)
    (description  . "This is a description")
    (event-source . #f)
    (transitions
     (,guard:#t ,action:no-op state-2)))
  (state->list (make <state>
                 #:name 'state-1
                 #:description "This is a description"
                 #:transitions `((,guard:#t ,action:no-op state-2)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "state")

(exit exit-status)


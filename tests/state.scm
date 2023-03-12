(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (smc context char)
             (smc fsm)
             (smc core transition)
             (smc core state)
             (tests common)
             (tests test-context))


(test-begin "state")
(configure-test-logging! "state")


;;; Accessors tests.

(test-equal "transition:guard"
  guard:#t
  (transition:guard `(,guard:#t ,action:no-op state-1)))

(test-equal "transition:action"
  action:no-op
  (transition:action `(,guard:#t ,action:no-op state-1)))

(test-equal "transition:next-state"
  'state-1
  (transition:next-state `(,guard:#t ,action:no-op state-1)))

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
    (transitions))
  (state->list (make <state> #:name 'state-1)))

(test-equal "state->list: With description"
  `((name         . state-1)
    (description  . "This is a description")
    (transitions))
  (state->list (make <state>
                 #:name 'state-1
                 #:description "This is a description")))

(test-equal "state->list: With transitions"
  `((name         . state-1)
    (transitions
     (,guard:#t ,action:no-op state-2)))
  (state->list (make <state>
                 #:name 'state-1
                 #:transitions `((,guard:#t ,action:no-op state-2)))))

(test-equal "state->list: With description and transitions"
  `((name         . state-1)
    (description  . "This is a description")
    (transitions
     (,guard:#t ,action:no-op state-2)))
  (state->list (make <state>
                 #:name 'state-1
                 #:description "This is a description"
                 #:transitions `((,guard:#t ,action:no-op state-2)))))



(test-equal "state->list: Check event source name"
  'test-event-source
  (let ((state (make <state>
                 #:event-source test-event-source
                 #:transitions `((,guard:#t ,action:no-op state-1)))))
    (state:event-source/name (state->list state))))

(test-equal "state->list/serialized"
  '((name       . #f)
    (event-source unquote test-event-source)
    (entry-action unquote entry-action)
    (transitions  ((unquote #{guard:#t}#) (unquote action:no-op) state-1)))
   (let ((state (make <state>
                  #:event-source test-event-source
                  #:entry-action entry-action
                  #:exit-action  exit-action
                  #:transitions `((,guard:#t ,action:no-op state-1)))))
    (state->list/serialized state)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "state")

(exit exit-status)


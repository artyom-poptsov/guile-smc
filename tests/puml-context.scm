(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (oop goops)
             (smc context char-context)
             (smc fsm)
             (smc puml-context)
             (smc core state)
             (smc core set)
             (tests test-context))


(define %test-name "puml-context")
(test-begin %test-name)



(test-equal "puml-context-resolved-procedures: check set content"
  test-event-source
  (let ((ctx (make <puml-context>
               #:port             (current-input-port)
               #:fsm-event-source 'test-event-source
               #:module           (list (resolve-module '(test-context))
                                        (current-module)))))
    (cdar (set-content (puml-context-resolved-procedures ctx)))))

(test-assert "puml-context-unresolved-procedures: empty set"
  (let ((ctx (make <puml-context>
               #:port             (current-input-port)
               #:fsm-event-source 'test-event-source
               #:module           (list (resolve-module '(test-context))
                                        (current-module)))))
    (set-empty? (puml-context-unresolved-procedures ctx))))

(test-error "initialize: Event source not found"
  'puml-error
  (let ((ctx (make <puml-context>
               #:port             (current-input-port)
               #:fsm-event-source 'some-procedure
               #:module           (list (resolve-module '(test-context))
                                        (current-module)))))
    ctx))



(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

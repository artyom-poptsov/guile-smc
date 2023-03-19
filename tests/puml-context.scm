(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (oop goops)
             (smc context char)
             (smc context oop char)
             (smc fsm)
             (smc puml-context)
             (smc core state)
             (smc core set)
             (tests common)
             (tests test-context))


(define %test-name "puml-context")

(configure-test-logging! %test-name)
(test-begin %test-name)



(test-equal "stanza->list-of-symbols"
  '(s1 s2)
  (stanza->list-of-symbols '((#\s #\2) (#\s #\1))))



(test-equal "puml-context-resolved-procedures: check set content"
  test-event-source
  (let ((ctx (make <puml-context>
               #:port             (current-input-port)
               #:fsm-event-source 'test-event-source
               #:module           (list (resolve-module '(test-context))
                                        (current-module)))))
    (cdr (hash-ref (puml-context-resolved-procedures ctx) 'test-event-source))))

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

(test-assert "resolve-procedure: failure"
  (let ((ctx (make <puml-context>
               #:port             (current-input-port)
               #:fsm-event-source 'test-event-source
               #:module           (list (resolve-module '(test-context))
                                        (current-module)))))
    (not (resolve-procedure ctx 'some-procedure))))

(test-assert "resolve-procedure: success"
  (let ((ctx (make <puml-context>
               #:port             (current-input-port)
               #:fsm-event-source 'test-event-source
               #:module           (list (resolve-module '(test-context))
                                        (current-module)))))
    (resolve-procedure ctx 'entry-action)))

(test-equal "module-name"
  "(test module)"
  (module-name '(test module)))



(test-assert "title?: #t"
  (let ((ctx (make <puml-context>
               #:port             (current-input-port)
               #:fsm-event-source 'test-event-source
               #:module           (list (resolve-module '(test-context))
                                        (current-module)))))
    (for-each (lambda (ch) (push-event-to-buffer ctx ch))
              (string->list "title"))
    (title? ctx #\space)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

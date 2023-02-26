(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (tests common)
             (smc context common))


(define %test-suite-name "context-common")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


(test-assert "guard:#t"
  (guard:#t 'context 'event))

(test-equal "action:no-op"
  'context
  (action:no-op 'context 'event))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc context context)
             (smc context oop generic))


(define %test-suite-name "context-oop-generic")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


;;; Guards.

(test-assert "context?"
  (context? (make <context>)))

(test-equal "context-debug-mode?"
  #f
  (context-debug-mode? (make <context>)))

(test-equal "context-debug-mode-set!"
  #t
  (let ((ctx (make <context>)))
    (context-debug-mode-set! ctx #t)
    (context-debug-mode? ctx)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


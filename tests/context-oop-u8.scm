(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc context oop u8))


(define %test-suite-name "context-oop-u8")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


;;; Guards.

(test-assert "u8-context?"
  (u8-context? (make <u8-context>)))

(test-equal "context-port"
  (current-input-port)
  (context-port (make <u8-context>)))

(test-equal "context-counter"
  0
  (context-counter (make <u8-context>)))

(test-equal "context-buffer"
  '()
  (context-buffer (make <u8-context>)))

(test-equal "context-stanza"
  '()
  (context-stanza (make <u8-context>)))

(test-equal "context-debug-mode?"
  #f
  (context-debug-mode? (make <u8-context>)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

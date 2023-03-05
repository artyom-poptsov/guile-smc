(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc context oop port))


(define %test-suite-name "context-oop-port")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


;;; Guards.

(test-assert "port-context?"
  (port-context? (make <port-context>)))

(test-equal "context-port"
  (current-input-port)
  (context-port (make <port-context>)))

(test-equal "context-counter"
  0
  (context-counter (make <port-context>)))

(test-equal "context-buffer"
  '()
  (context-buffer (make <port-context>)))

(test-equal "context-stanza"
  '()
  (context-stanza (make <port-context>)))

(test-equal "context-debug-mode?"
  #f
  (context-debug-mode? (make <port-context>)))

(test-equal "context-counter-update!: +1"
  1
  (let ((ctx (make <port-context>)))
    (context-counter-update! ctx)
    (context-counter ctx)))

(test-equal "context-counter-update!: +2"
  2
  (let ((ctx (make <port-context>)))
    (context-counter-update! ctx 2)
    (context-counter ctx)))

(test-equal "update-counter"
  1
  (let ((ctx (make <port-context>)))
    (update-counter ctx 'event)
    (context-counter ctx)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

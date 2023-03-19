(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 binary-ports)
             (oop goops)
             (tests common)
             (smc context functional port))


(define %test-suite-name "context-functional-port")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)

(test-assert "%make-port-context"
  (%make-port-context #f                   ; parent-context
                      (current-input-port))) ; port

(test-assert "make-port-context"
  (make-port-context #:port (current-input-port)
                     #:debug-mode? #f
                     #:counter 0
                     #:buffer '()
                     #:stanza '()
                     #:result '()))

(test-equal "update-counter-update"
  2
  (context-counter (context-counter-update (make-port-context)
                                           #:delta 2)))

(test-equal "context-stanza"
  '()
  (context-stanza (make-port-context)))


(test-equal "buffer-empty?: #t"
  #t
  (buffer-empty? (make-port-context)))

(test-equal "stanza-empty?: #t"
  #t
  (stanza-empty? (make-port-context)))

(test-equal "result-empty?: #t"
  #t
  (result-empty? (make-port-context)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


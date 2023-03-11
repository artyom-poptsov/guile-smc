(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 binary-ports)
             (oop goops)
             (tests common)
             (smc context functional generic))


(define %test-suite-name "context-functional-generic")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)

(test-assert "%make-context"
  (%make-context #f                   ; debug-mode?
                 0                    ; counter
                 '()                  ; buffer
                 '()                  ; stanza
                 '()))                ; result

(test-assert "make-context"
  (make-context #:debug-mode? #f
                #:counter 0
                #:buffer '()
                #:stanza '()
                #:result '()))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


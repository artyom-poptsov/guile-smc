(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 binary-ports)
             (oop goops)
             (tests common)
             (smc context functional u8))


(define %test-suite-name "context-functional-u8")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)

(test-assert "%make-u8-context"
  (%make-u8-context #f                   ; debug-mode?
                    (current-input-port) ; port
                    0                    ; counter
                    '()                  ; buffer
                    '()                  ; stanza
                    '()))                ; result


(test-assert "make-u8-context"
  (make-u8-context #:port (current-input-port)
                   #:debug-mode? #f
                   #:counter 0
                   #:buffer '()
                   #:stanza '()
                   #:result '()))

(test-equal "update-counter-update"
  2
  (context-counter (context-counter-update (make-u8-context)
                                           #:delta 2)))

(test-equal "context-stanza"
  '()
  (context-stanza (make-u8-context)))


(test-equal "buffer-empty?: #t"
  #t
  (buffer-empty? (make-u8-context)))

(test-equal "stanza-empty?: #t"
  #t
  (stanza-empty? (make-u8-context)))

(test-equal "result-empty?: #t"
  #t
  (result-empty? (make-u8-context)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


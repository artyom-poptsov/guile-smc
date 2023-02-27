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
  (%make-port-context (current-input-port) ; port
                      #f                   ; debug-mode?
                      0                    ; counter
                      '()                  ; buffer
                      '()                  ; stanza
                      '()))                ; result

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

(test-equal "context-buffer-append"
  '(c b a)
  (context-buffer (context-buffer-append
                   (context-buffer-append
                    (context-buffer-append (make-port-context) 'a)
                    'b)
                   'c)))

(test-equal "context-buffer/reversed"
  '(a b c)
  (context-buffer/reversed
   (append-to-buffer
    (append-to-buffer
     (append-to-buffer (make-port-context) 'a)
     'b)
    'c)))

(test-equal "context-stanza"
  '()
  (context-stanza (make-port-context)))

(test-equal "context-stanza-append"
  '(c b a)
  (context-stanza
   (context-stanza-append
    (context-stanza-append
     (context-stanza-append (make-port-context) 'a)
     'b)
    'c)))

(test-equal "context-stanza/reversed"
  '(a b c)
  (context-stanza/reversed
   (context-stanza-append
    (context-stanza-append
     (context-stanza-append (make-port-context) 'a)
     'b)
    'c)))

(test-equal "context-result"
  '()
  (context-result (make-port-context)))

(test-equal "context-result-append"
  '(c b a)
  (context-result
   (context-result-append
    (context-result-append
     (context-result-append (make-port-context) 'a)
     'b)
    'c)))

(test-equal "context-result/reversed"
  '(a b c)
  (context-result/reversed
   (context-result-append
    (context-result-append
     (context-result-append (make-port-context) 'a)
     'b)
    'c)))


;;; Actions.

(test-equal "update-counter"
  1
  (context-counter (update-counter (make-port-context))))

(test-equal "reset-buffer"
  '()
  (context-buffer
   (reset-buffer
    (append-to-buffer (make-port-context) 'a)
    #f)))

(test-equal "reset-stanza"
  '()
  (context-stanza
   (reset-stanza
    (context-stanza-append (make-port-context) 'a))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


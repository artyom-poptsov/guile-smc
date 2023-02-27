(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 binary-ports)
             (oop goops)
             (tests common)
             (smc context functional char))


(define %test-suite-name "context-functional-char")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)

(test-assert "%make-char-context"
  (%make-char-context (current-input-port) ; port
                      #f                   ; debug-mode?
                      0                    ; counter
                      0                    ; row-number
                      0                    ; col-number
                      '()                  ; buffer
                      '()                  ; stanza
                      '()))                ; result

(test-assert "make-char-context"
  (make-char-context #:port (current-input-port)
                     #:debug-mode? #f
                     #:counter 0
                     #:row-number 0
                     #:col-number 0
                     #:buffer '()
                     #:stanza '()
                     #:result '()))

(test-equal "context-buffer: initially must be an empty list"
  '()
  (context-buffer (make-char-context)))

(test-equal "context-stanza: initially must be an empty list"
  '()
  (context-stanza (make-char-context)))

(test-equal "context-result: initially must be an empty list"
  '()
  (context-result (make-char-context)))

(test-equal "context-counter-update"
  1
  (let ((ctx (make-char-context)))
    (context-counter (context-counter-update ctx))))

(test-equal "context-buffer-append"
  '(c b a)
  (context-buffer (context-buffer-append
                   (context-buffer-append
                    (context-buffer-append (make-char-context) 'a)
                    'b)
                   'c)))

(test-equal "context-buffer/reversed"
  '(a b c)
  (context-buffer/reversed
   (context-buffer-append
    (context-buffer-append
     (context-buffer-append (make-char-context) 'a)
     'b)
    'c)))

(test-equal "reset-buffer"
  '()
  (context-buffer
   (reset-buffer
    (context-buffer-append (make-char-context) 'a))))

(test-equal "context-stanza"
  '()
  (context-stanza (make-char-context)))

(test-equal "context-stanza-append"
  '(c b a)
  (context-stanza
   (context-stanza-append
    (context-stanza-append
     (context-stanza-append (make-char-context) 'a)
     'b)
    'c)))

(test-equal "context-stanza/reversed"
  '(a b c)
  (context-stanza/reversed
   (context-stanza-append
    (context-stanza-append
     (context-stanza-append (make-char-context) 'a)
     'b)
    'c)))

(test-equal "reset-stanza"
  '()
  (context-stanza
   (reset-stanza
    (context-stanza-append (make-char-context) 'a))))

(test-equal "context-result-append"
  '(c b a)
  (context-result
   (context-result-append
    (context-result-append
     (context-result-append (make-char-context) 'a)
     'b)
    'c)))

(test-equal "context-result/reversed"
  '(a b c)
  (context-result/reversed
   (context-result-append
    (context-result-append
     (context-result-append (make-char-context) 'a)
     'b)
    'c)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


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
                      0                    ; row-number
                      0                    ; col-number
                      0                    ; counter
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

(test-equal "context-buffer/reversed"
  '(a b c)
  (context-buffer/reversed (make-char-context #:buffer '(c b a))))

(test-equal "context-stanza"
  '()
  (context-stanza (make-char-context)))

(test-equal "context-stanza/reversed"
  '(a b c)
  (context-stanza/reversed (make-char-context #:stanza '(c b a))))

(test-equal "context-result/reversed"
  '(a b c)
  (context-result/reversed (make-char-context #:result '(c b a))))


;;; clear

(test-equal "clear-buffer"
  '()
  (context-buffer (clear-buffer (make-char-context #:buffer '(a b c)) 'event)))

(test-equal "clear-stanza"
  '()
  (context-stanza (clear-stanza (make-char-context #:stanza '(a b c)) 'event)))

(test-equal "clear-result"
  '()
  (context-result (clear-result (make-char-context #:stanza '(a b c)) 'event)))


(test-equal "buffer-empty?: #t"
  #t
  (buffer-empty? (make-char-context)))

(test-equal "stanza-empty?: #t"
  #t
  (stanza-empty? (make-char-context)))

(test-equal "result-empty?: #t"
  #t
  (result-empty? (make-char-context)))


;;; reverse

(test-equal "reverse-buffer"
  '(a b c)
  (context-buffer (reverse-buffer (make-char-context #:buffer '(c b a)))))

(test-equal "reverse-stanza"
  '(a b c)
  (context-stanza (reverse-stanza (make-char-context #:stanza '(c b a)))))

(test-equal "reverse-result"
  '(a b c)
  (context-result (reverse-result (make-char-context #:result '(c b a)))))


;;; push

(test-equal "push-event-to-buffer"
  '(a b c)
  (context-buffer (push-event-to-buffer (make-char-context #:buffer '(b c))
                                        'a)))

(test-equal "push-event-to-stanza"
  '(event)
  (context-stanza (push-event-to-stanza (make-char-context) 'event)))

(test-equal "push-event-to-result"
  '(event)
  (context-result (push-event-to-result (make-char-context) 'event)))

(test-equal "push-buffer-to-stanza"
  '((a b c))
  (context-stanza (push-buffer-to-stanza (make-char-context #:buffer '(a b c))
                                         'event)))


;;; pop
(test-equal "pop-buffer"
  '(b c)
  (context-buffer (pop-buffer (make-char-context #:buffer '(a b c)) 'event)))

(test-equal "pop-stanza"
  '(b c)
  (context-stanza (pop-stanza (make-char-context #:stanza '(a b c)) 'event)))

(test-equal "pop-result"
  '(b c)
  (context-result (pop-result (make-char-context #:result '(a b c)) 'event)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


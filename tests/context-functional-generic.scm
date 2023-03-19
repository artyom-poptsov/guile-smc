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


(test-assert "context?"
  (context? (make-context)))

(test-assert "context-debug-mode?"
  (context-debug-mode? (make-context #:debug-mode? #t)))

(test-equal "context-counter"
  0
  (context-counter (make-context)))

(test-equal "context-buffer"
  '()
  (context-buffer (make-context)))

(test-equal "context-buffer/reversed"
  '(a b c)
  (context-buffer/reversed (make-context #:buffer '(c b a))))

(test-equal "context-stanza"
  '()
  (context-stanza (make-context)))

(test-equal "context-stanza/reversed"
  '(a b c)
  (context-stanza/reversed (make-context #:stanza '(c b a))))

(test-equal "context-result"
  '()
  (context-result (make-context)))

(test-equal "context-result/reversed"
  '(a b c)
  (context-result/reversed (make-context #:result '(c b a))))

(test-equal "context-counter-update: +1"
  1
  (context-counter (context-counter-update (make-context))))

(test-equal "context-counter-update: +2"
  2
  (context-counter (context-counter-update (make-context) 2)))



(test-equal "clear-buffer"
  '()
  (context-buffer (clear-buffer (make-context #:buffer '(a b c)) 'event)))

(test-equal "clear-stanza"
  '()
  (context-stanza (clear-stanza (make-context #:stanza '(a b c)) 'event)))

(test-equal "clear-result"
  '()
  (context-result (clear-result (make-context #:stanza '(a b c)) 'event)))



(test-equal "buffer-empty?: #t"
  #t
  (buffer-empty? (make-context)))

(test-equal "stanza-empty?: #t"
  #t
  (stanza-empty? (make-context)))

(test-equal "result-empty?: #t"
  #t
  (result-empty? (make-context)))



(test-equal "reverse-buffer"
  '(a b c)
  (context-buffer (reverse-buffer (make-context #:buffer '(c b a)))))

(test-equal "reverse-stanza"
  '(a b c)
  (context-stanza (reverse-stanza (make-context #:stanza '(c b a)))))

(test-equal "reverse-result"
  '(a b c)
  (context-result (reverse-result (make-context #:result '(c b a)))))



(test-equal "push-event-to-buffer"
  '(event)
  (context-buffer (push-event-to-buffer (make-context) 'event)))

(test-equal "push-event-to-stanza"
  '(event)
  (context-stanza (push-event-to-stanza (make-context) 'event)))

(test-equal "push-event-to-result"
  '(event)
  (context-result (push-event-to-result (make-context) 'event)))

(test-equal "push-buffer-to-stanza"
  '((a b c))
  (context-stanza (push-buffer-to-stanza (make-context #:buffer '(a b c))
                                         'event)))



(test-equal "pop-buffer"
  '(b c)
  (context-buffer (pop-buffer (make-context #:buffer '(a b c)) 'event)))

(test-equal "pop-stanza"
  '(b c)
  (context-stanza (pop-stanza (make-context #:stanza '(a b c)) 'event)))

(test-equal "pop-result"
  '(b c)
  (context-result (pop-result (make-context #:result '(a b c)) 'event)))

(test-error "throw-error"
  'misc-error
  (throw-error (make-context) 'event))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


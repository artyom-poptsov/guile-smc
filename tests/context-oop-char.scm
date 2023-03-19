(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc context oop char))


(define %test-suite-name "context-oop-char")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


;;; Guards.

(test-assert "char-context?"
  (char-context? (make <char-context>)))

(test-equal "context-port"
  (current-input-port)
  (context-port (make <char-context>)))

(test-equal "context-counter"
  0
  (context-counter (make <char-context>)))

(test-equal "context-buffer"
  '()
  (context-buffer (make <char-context>)))

(test-equal "context-buffer/reversed"
  '(a b c)
  (context-buffer/reversed (make <char-context> #:buffer '(c b a))))

(test-equal "context-stanza"
  '()
  (context-stanza (make <char-context>)))

(test-equal "context-stanza/reversed"
  '(a b c)
  (context-stanza/reversed (make <char-context> #:stanza '(c b a))))

(test-equal "context-debug-mode?"
  #f
  (context-debug-mode? (make <char-context>)))

(test-equal "context-counter-update!: +1"
  1
  (let ((ctx (make <char-context>)))
    (context-counter-update! ctx)
    (context-counter ctx)))

(test-equal "context-counter-update!: +2"
  2
  (let ((ctx (make <char-context>)))
    (context-counter-update! ctx 2)
    (context-counter ctx)))

(test-equal "push-event-to-buffer"
  '(event)
  (context-buffer (push-event-to-buffer (make <char-context>) 'event)))

(test-equal "push-event-to-stanza"
  '(event)
  (context-stanza (push-event-to-stanza (make <char-context>) 'event)))

(test-equal "push-event-to-result"
  '(event)
  (context-result (push-event-to-result (make <char-context>) 'event)))

(test-equal "push-buffer-to-stanza"
  '((a b c))
  (context-stanza
   (push-buffer-to-stanza
    (push-event-to-buffer
     (push-event-to-buffer
      (push-event-to-buffer (make <char-context>) 'a)
      'b)
     'c)
    'event)))

(test-equal "push-stanza-to-result"
  '(((a b c)))
  (context-result
   (push-stanza-to-result
    (push-buffer-to-stanza
     (push-event-to-buffer
      (push-event-to-buffer
       (push-event-to-buffer (make <char-context>) 'a)
       'b)
      'c)
     'event)
    'event)))

(test-equal "buffer-empty?: #t"
  #t
  (buffer-empty? (make <char-context>)))

(test-equal "buffer-empty?: #f"
  #f
  (buffer-empty? (push-event-to-buffer (make <char-context>) 'event)))

(test-equal "update-counter"
  1
  (let ((ctx (make <char-context>)))
    (update-counter ctx 'event)
    (context-counter ctx)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

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

(test-equal "context-buffer/reversed"
  '(a b c)
  (context-buffer/reversed (make <port-context> #:buffer '(c b a))))

(test-equal "context-stanza"
  '()
  (context-stanza (make <port-context>)))

(test-equal "context-stanza/reversed"
  '(a b c)
  (context-stanza/reversed (make <port-context> #:stanza '(c b a))))

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

(test-equal "push-event-to-buffer"
  '(event)
  (context-buffer (push-event-to-buffer (make <port-context>) 'event)))

(test-equal "push-event-to-stanza"
  '(event)
  (context-stanza (push-event-to-stanza (make <port-context>) 'event)))

(test-equal "push-event-to-result"
  '(event)
  (context-result (push-event-to-result (make <port-context>) 'event)))

(test-equal "push-buffer-to-stanza"
  '((c b a))
  (context-stanza
   (push-buffer-to-stanza
    (push-event-to-buffer
     (push-event-to-buffer
      (push-event-to-buffer (make <port-context>) 'a)
      'b)
     'c)
    'event)))

(test-equal "buffer-empty?: #t"
  #t
  (buffer-empty? (make <port-context>)))

(test-equal "buffer-empty?: #f"
  #f
  (buffer-empty? (push-event-to-buffer (make <port-context>) 'event)))

(test-equal "update-counter"
  1
  (let ((ctx (make <port-context>)))
    (update-counter ctx 'event)
    (context-counter ctx)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

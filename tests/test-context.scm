(define-module (test-context)
  #:export (test-event-source
            entry-action
            exit-action))

(define (test-event-source ctx)
  #t)

(define (entry-action ctx)
  ctx)

(define (exit-action ctx)
  ctx)

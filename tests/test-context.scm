(define-module (tests test-context)
  #:export (test-event-source
            entry-action
            exit-action
            pre-action
            post-action))

(define (pre-action ctx event)
  ctx)

(define (post-action ctx event)
  ctx)

(define (test-event-source ctx)
  #t)

(define (entry-action ctx)
  ctx)

(define (exit-action ctx)
  ctx)

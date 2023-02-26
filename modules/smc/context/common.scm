(define-module (smc context common)
  #:export (guard:#t
            action:no-op))


(define (action:no-op ctx event)
  "The action that does nothing special, just returns the context CTX as it is."
  ctx)

(define (guard:#t ctx event)
  "This guard is always returns #t."
  #t)

;;; common.scm ends here.

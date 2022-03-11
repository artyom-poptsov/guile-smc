(define-module (context)
  #:use-module (ice-9 textual-ports))

(define-public (guard:non-space? ctx ch)
  (not (char=? ch #\space)))

(define-public (action:end ctx ch)
  (list->string (reverse ctx)))

(define-public (action:cons ctx ch)
  (cons ch ctx))

(define-public (custom-event-source context)
  (get-char (current-input-port)))

(define-module (context)
  #:use-module (oop goops)
  #:use-module (ice-9 textual-ports)
  #:use-module (smc context char-context)
  #:re-export (guard:#t
               guard:semicolon?
               guard:double-quote?
               guard:newline?
               guard:eof-object?
               action:no-op)
  #:export (event-source
            action:count
            action:validate))

(define-method (event-source (ctx <number>))
  (get-char (current-input-port)))

(define (action:count ctx char)
  (cond
   ((char=? char #\()
    (+ ctx 1))
   ((char=? char #\))
    (- ctx 1))
   (else
    ctx)))

(define (action:validate ctx char)
  (unless (zero? ctx)
    (error "Parenthesis mismatch" ctx))
  ctx)

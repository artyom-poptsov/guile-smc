(define-module (context)
  #:use-module (oop goops)
  #:use-module (ice-9 textual-ports)
  #:use-module (smc context char)
  #:re-export (guard:#t
               char:semicolon?
               char:double-quote?
               char:newline?
               char:eof-object?
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

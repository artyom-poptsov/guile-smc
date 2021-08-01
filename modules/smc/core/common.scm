(define-module (smc core common)
  #:export (object-address/hex-string
            safe-module-ref))

(define (safe-module-ref module proc-name)
  (catch #t
    (lambda ()
      (module-ref module proc-name))
    (const #f)))

(define (object-address/hex-string object)
  (number->string (object-address object) 16))

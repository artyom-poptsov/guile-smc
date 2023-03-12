(define-module (png-signature-context)
  #:use-module (smc context char)
  #:re-export (char:letter-P?
               char:letter-N?
               char:letter-G?
               char:cr?
               char:lf?
               char:sub?)               ; Ctrl+Z
  #:export(guard:correct-first-byte?
           action:wrong-first-byte-error
           action:unexpected-eof-error
           action:unexpected-byte-error))

(define-public (correct-first-byte? ctx byte)
  (equal? byte 137))

(define (throw-wrong-first-byte-error ctx byte)
  (error "Wrong first byte" ctx byte))

(define (throw-unexpected-eof-error ctx byte)
  (error "Unexpected end of file" ctx byte))

(define (throw-unexpected-byte-error ctx byte)
  (error "Unexpected byte read" ctx byte))

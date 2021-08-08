(define-module (png-signature-context)
  #:export (guard:correct-first-byte?
            guard:letter-P?
            guard:letter-N?
            guard:letter-G?
            guard:letter-cr?
            guard:letter-lf?
            guard:letter-ctrl-z?
            guard:letter-lf?

            action:wrong-first-byte-error
            action:unexpected-eof-error
            action:unexpected-byte-error))

(define-public (guard:correct-first-byte? ctx byte)
  (equal? byte 137))

(define-public (guard:letter-P? ctx byte)
  (equal? (integer->char byte) #\P))

(define-public (guard:letter-N? ctx byte)
  (equal? (integer->char byte) #\N))

(define-public (guard:letter-G? ctx byte)
  (equal? (integer->char byte) #\G))

(define-public (guard:letter-LF? ctx byte)
  (equal? (integer->char byte) #\linefeed))

(define-public (guard:letter-CR? ctx byte)
  (equal? (integer->char byte) #\return))

(define-public (guard:letter-ctrl-z? ctx byte)
  (equal? (integer->char byte) #\032))


(define (action:wrong-first-byte-error ctx byte)
  (error "Wrong first byte" ctx byte))

(define (action:unexpected-eof-error ctx byte)
  (error "Unexpected end of file" ctx byte))

(define (action:unexpected-byte-error ctx byte)
  (error "Unexpected byte read" ctx byte))

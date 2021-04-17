(define-module (smc core log)
  #:use-module (oop goops)
  #:export (log
            log-error
            log-warning
            log-info
            log-debug
            log-use-stderr!))

(define %logger "/usr/bin/logger")
(define %tag    "guile-smc")

(define *use-stderr?* #f)


(define-method (log-use-stderr! (value <boolean>))
  (set! *use-stderr?* value))

(define (log level fmt . args)
  (let* ((message (apply format #f fmt args))
         (command (format #f "~a ~a --priority=user.~a --tag='~a' '~a'"
                          %logger
                          (if *use-stderr?*
                              "--stderr"
                              "")
                          level
                          %tag
                          message))
         (result (system command)))
    (unless (zero? result)
      (error "Could not log a message"))))

(define (log-error fmt . args)
  (apply log "err" fmt args))

(define (log-warning fmt . args)
  (apply log "warning" fmt args))

(define (log-info fmt . args)
  (apply log "info" fmt args))

(define (log-debug fmt . args)
  (apply log "debug" fmt args))


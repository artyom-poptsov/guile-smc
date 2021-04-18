#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (smc puml))

(define (main args)
  (format #t "result: ~a~%" (puml->fsm (current-input-port))))

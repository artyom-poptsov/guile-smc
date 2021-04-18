#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (ice-9 pretty-print)
             (smc puml)
             (smc fsm))



(define (main args)
  (let ((fsm (puml->fsm (current-input-port))))
    (format #t "output fsm: ~a~%" fsm)
    (format #t "transition table:~%")
    (pretty-print
     (hash-table->transition-list (fsm-transition-table fsm))
     #:display? #t)))

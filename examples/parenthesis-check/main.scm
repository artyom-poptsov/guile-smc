#!/usr/bin/guile \
-L . -e main -s
!#

(use-modules (oop goops)
             (smc fsm)
             (custom-fsm))

(define (main args)
  (let ((fsm (make <custom-fsm>)))
    (fsm-run! fsm 0)))

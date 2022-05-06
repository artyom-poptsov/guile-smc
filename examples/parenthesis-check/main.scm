#!/home/avp/.guix-profile/bin/guile \
-L . -e main -s
!#

(use-modules (oop goops)
             (custom-fsm))

(define (main args)
  (display (run-fsm 0))
  (newline))

#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (ice-9 pretty-print)
             (smc puml)
             (smc fsm))



(define (main args)
  (let ((fsm (puml->fsm (current-input-port)
                        #:module (list (resolve-module '(smc context char-context))
                                       (resolve-module '(smc puml))
                                       (resolve-module '(smc fsm)))
                        #:debug-mode? #f)))
    (format #t "output fsm:             ~a~%" fsm)
    (format #t "  state count: ~s~%" (fsm-state-count fsm))
    (format #t "  validation result: ~%")
    (pretty-print (fsm-validate fsm))
    (format #t "transition table (count: ~a):~%" (fsm-transition-count fsm))
    (pretty-print
     (hash-table->transition-list (fsm-transition-table fsm))
     #:display? #t)))

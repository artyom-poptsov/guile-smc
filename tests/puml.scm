(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (oop goops)
             (smc guards char)
             (smc fsm)
             (smc puml)
             (smc core state))


(test-begin "puml")

(test-error "puml-string->fsm: error: no @startuml"
  #t
  (puml-string->fsm (string-join
                     (list
                      "[*] -> state_1\n"
                      "@enduml\n"))))

(test-assert "puml-string->fsm: first state"
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "[*] -> state_1\n"
                                 "@enduml\n")))))
    (equal? (fsm-current-state fsm) (fsm-state fsm 'state_1))))

(test-assert "puml-string->fsm: a transition with guard"
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "[*] -> state_1\n"
                                 "state_1 --> state_1: guard:#t\n"
                                 "@enduml\n")))))
    (and (equal? (fsm-current-state fsm) (fsm-state fsm 'state_1))
         (equal? guard:#t
                 (car (car (state-transitions
                                   (fsm-state fsm 'state_1))))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "puml")

(exit exit-status)

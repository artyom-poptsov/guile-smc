(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (oop goops)
             (smc context char-context)
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

(test-assert "puml-string->fsm: a transition with an action"
  (let* ((fsm (puml-string->fsm (string-join
                                 (list
                                  "@startuml\n"
                                  "[*] -> state_1\n"
                                  "state_1 --> state_1: guard:#t -> action:no-op\n"
                                  "@enduml\n"))))
         (state (fsm-state fsm 'state_1)))
    (and (equal? (fsm-current-state fsm) state)
         (equal? guard:#t (car (car (state-transitions state))))
         (equal? action:no-op (cadr (car (state-transitions state)))))))

(test-equal "puml-string->fsm: a state with description"
  "A state description."
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "[*] -> state_1\n"
                                 "state_1: A state description.\n"
                                 "state_1 --> state_1: guard:#t\n"
                                 "@enduml\n")))))
    (state-description (fsm-state fsm 'state_1))))

(test-equal "puml-string->fsm: an FSM with description"
  "This is an FSM description."
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "title This is an FSM description.\n"
                                 "[*] -> state_1\n"
                                 "state_1: A state description.\n"
                                 "state_1 --> state_1: guard:#t\n"
                                 "@enduml\n")))))
    (fsm-description fsm)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "puml")

(exit exit-status)

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (oop goops)
             (smc context char)
             (smc context oop char)
             (smc fsm)
             (smc puml)
             (smc core state)
             (smc core log)
             (tests common)
             (tests test-context))


(define %test-suite-name "puml")

(test-begin %test-suite-name)

(configure-test-logging! %test-suite-name)

(define event-source char-context-event-source)


;; read-start-tag errors

(test-error "puml-string->fsm: error: empty file"
  #t
  (puml-string->fsm ""))

(test-error "puml-string->fsm: error: no @startuml"
  #t
  (puml-string->fsm (string-join
                     (list
                      "[*] -> state_1\n"
                      "@enduml\n"))))

(test-error "puml-string->fsm: error: @startuml is misspelled"
  #t
  (puml-string->fsm (string-join
                     (list
                      "@statruml"
                      "[*] -> state_1\n"
                      "@enduml\n"))))



(test-assert "puml-string->fsm: minimal example, @startuml ends with a newline"
  (puml-string->fsm (string-join
                     (list
                      "@startuml\n"
                      "@enduml\n"))))

(test-assert "puml-string->fsm: minimal example, @startuml ends with a space and newline"
  (puml-string->fsm (string-join
                     (list
                      "@startuml \n"
                      "@enduml\n"))))

(test-assert "puml-string->fsm: first state"
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "[*] -> state_1\n"
                                 "@enduml\n"))
                               #:debug-mode? #f)))
    (equal? (fsm-current-state fsm) (fsm-state fsm 'state_1))))

(test-assert "puml-string->fsm: a transition with guard"
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "[*] -> state_1\n"
                                 "state_1 --> state_1: guard:#t\n"
                                 "@enduml\n"))
                               #:debug-mode? #f)))
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
                                  "@enduml\n"))
                                #:debug-mode? #f))
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



(test-equal "puml-string->fsm: an FSM with event source"
  test-event-source
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "title This is an FSM description.\n"
                                 "[*] -> state_1\n"
                                 "state_1: A state description.\n"
                                 "state_1: event-source: test-event-source\n"
                                 "state_1 --> state_1: guard:#t\n"
                                 "@enduml\n"))
                               #:module (list (resolve-module '(test-context))
                                              (current-module))
                               #:debug-mode? #f)))
    (state-event-source (fsm-state fsm 'state_1))))

(test-equal "puml-string->fsm: an FSM with an entry action"
  entry-action
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "title This is an FSM description.\n"
                                 "[*] -> state_1\n"
                                 "state_1: A state description.\n"
                                 "state_1: entry-action: entry-action\n"
                                 "state_1 --> state_1: guard:#t\n"
                                 "@enduml\n"))
                               #:module (list (resolve-module '(test-context))
                                              (current-module))
                               #:debug-mode? #f)))
    (state-entry-action (fsm-state fsm 'state_1))))

(test-equal "puml-string->fsm: an FSM with an exit action"
  exit-action
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml\n"
                                 "title This is an FSM description.\n"
                                 "[*] -> state_1\n"
                                 "state_1: A state description.\n"
                                 "state_1: exit-action: exit-action\n"
                                 "state_1 --> state_1: guard:#t\n"
                                 "@enduml\n"))
                               #:module (list (resolve-module '(test-context))
                                              (current-module))
                               #:debug-mode? #f)))
    (state-exit-action (fsm-state fsm 'state_1))))



(test-error "legend: no 'endlegend' error"
  #t
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml"
                                 "title This is an FSM description."
                                 "legend"
                                 "[*] -> state_1"
                                 "state_1: A state description."
                                 "state_1: exit-action: exit-action"
                                 "state_1 --> state_1: guard:#t"
                                 "@enduml")
                                "\n")
                               #:module (list (resolve-module '(test-context))
                                              (current-module))
                               #:debug-mode? #t)))
    fsm))

(test-equal "legend: event-source"
  test-event-source
  (let ((fsm (puml-string->fsm (string-join
                                (list
                                 "@startuml"
                                 "title This is an FSM description."
                                 "legend"
                                 "event-source: test-event-source"
                                 "endlegend"
                                 "[*] -> state_1"
                                 "state_1: A state description."
                                 "state_1: exit-action: exit-action"
                                 "state_1 --> state_1: guard:#t"
                                 "@enduml")
                                "\n")
                               #:module (list (resolve-module '(test-context))
                                              (current-module))
                               #:debug-mode? #t)))
    (fsm-event-source fsm)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "puml")

(exit exit-status)

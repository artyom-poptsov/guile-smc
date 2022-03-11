(use-modules (oop goops)
             (ice-9 textual-ports)
             (smc fsm)
             (smc context char-context)
             (smc compiler)
             (context))

(let ((fsm (make <fsm>
             #:event-source custom-event-source
             #:description "This FSM reads the first word and skips everything else."
             #:transition-table
             `(((name . before)
                (description . "Search for a word.")
                (transitions
                 (,guard:eof-object? ,action:end   #f)
                 (,guard:non-space?  ,action:cons  inside)
                 (,guard:space?      ,action:no-op before)))
               ((name . inside)
                (description . "Read a word.")
                (transitions
                 (,guard:eof-object? ,action:end   #f)
                 (,guard:space?      ,action:no-op after)
                 (,guard:non-space?  ,action:cons  inside)))
               ((name . after)
                (description . "Skip everything after a word.")
                (transitions
                 (,guard:eof-object? ,action:end   #f)
                 (,guard:newline?    ,action:cons  before)
                 (,guard:#t          ,action:no-op after)))))))
  (fsm-compile fsm
               #:fsm-name   'fsm
               #:fsm-module '(fsm)
               #:extra-modules '((context))
               #:target 'guile-standalone))

(define-module (smc cli command-run)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 r5rs)
  #:use-module (smc core log)
  #:use-module (smc core set)
  #:use-module (smc compiler)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc cli common)
  #:export (command-run))

(define (print-help)
  (display "\
Usage: smc run [options] <puml-file>

Run a state machine.

Options:
  --help, -h        Print this message and exit.
  --eval, -e <procedure>
                    Eval a procedure with the resulting context as a parameter.
                    Example value:
                      \"(lambda (context) (display context))\"
  --load-path, -L <load-path>
                    Add an extra load path.
  --context-thunk, -C <procedure>
                    A thunk that produces the initial value for an FSM context.
                    Example value: \"(lambda () 0)\"
  --modules, -U <modules>
                    Load additional modules.  The value must be the same
                    as for 'use-modules'.  Example value:
                      \"((smc context char-context) (smc puml-context))\"
  --validate        Validate the output FSM and print the validation result.
                    The exit code is 0 if the validation is passed,
                    and a non-zero value otherwise.
  --debug           Enable the debug mode.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (eval                     (single-char #\e) (value #t))
    (load-path                (single-char #\L) (value #t))
    (context                  (single-char #\C) (value #t))
    (modules                  (single-char #\U) (value #t))
    (validate                                   (value #f))
    (debug                                      (value #f))))



(define %default-result-handler
  "(lambda (context) (display context) (newline))")

(define %default-context-thunk
  "(lambda () #f)")



(define (command-run args)
  (let* ((options          (getopt-long args %option-spec))
         (eval-proc        (option-ref options
                                       'eval
                                       %default-result-handler))
         (extra-load-paths (option-ref options 'load-path ""))
         (context-thunk    (option-ref options
                                       'context
                                       %default-context-thunk))
         (modules          (option-ref options 'modules   "()"))
         (debug-mode?      (option-ref options 'debug     #f))
         (args             (option-ref options '()        #f)))

    (when (or (option-ref options 'help #f) (null? args))
      (print-help)
      (exit 0))

    (log-use-stderr! debug-mode?)
    (add-to-load-path* (string-split extra-load-paths #\:))

    (let* ((port    (open-input-file (car args)))
           (fsm     (puml->fsm port
                               #:module      (puml-modules modules)
                               #:debug-mode? debug-mode?)))
      (when (option-ref options 'validate #f)
        (let ((validation-result (fsm-validate fsm)))
          (unless (null? validation-result)
            (pretty-print validation-result (current-error-port))
            (exit 1))))

      (let* ((extra-modules `(,(resolve-module '(smc fsm))
                              ,@(map (lambda (m) (resolve-module m))
                                     (eval-string/quote modules))))
             (env           (null-environment 5)))
        (for-each (lambda (module)
                    (module-use! env module))
                  extra-modules)
        (module-define! env 'fsm fsm)

        (eval
         `(begin
            (fsm-event-source-set! fsm event-source)
            (let ((context (fsm-run! fsm ((eval-string ,context-thunk)))))
              ((eval-string ,eval-proc) context)))
         env)))))

;;; command-run.scm ends here.


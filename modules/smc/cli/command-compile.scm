(define-module (smc cli command-compile)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (smc core log)
  #:use-module (smc compiler)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc cli common)
  #:export (command-compile))


(define (print-compile-help)
  (display "\
Usage: smc compile [options]

The program reads a PlantUML transition diagram from the standard input.

Options:
  --help, -h        Print this message and exit.
  --print-transition-table, -p
                    Print the FSM transition table.
  --load-path, -L   Add a paths separated by a colon to load paths.
  --modules, -U     Load additional modules.
  --fsm-name, -n    Set the name for the output FSM.
  --fsm-module, -m  Set the module for the output FSM.
  --validate        Validate the output FSM and print the validation result.
  --debug           Enable the debug mode.

"))


(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (print-transition-table   (single-char #\p) (value #f))
    (load-path                (single-char #\L) (value #t))
    (modules                  (single-char #\U) (value #t))
    (fsm-name                 (single-char #\n) (value #t))
    (fsm-module               (single-char #\m) (value #t))
    (validate                                   (value #f))
    (debug                                      (value #f))))

(define (command-compile args)
  (let* ((options          (getopt-long args %option-spec))
         (extra-load-paths (option-ref options 'load-path ""))
         (debug-mode?      (option-ref options 'debug     #f)))

    (when (option-ref options 'help #f)
      (print-compile-help)
      (exit 0))

    (log-use-stderr! debug-mode?)

    (add-to-load-path* (string-split extra-load-paths #\:))

    (let* ((modules      (option-ref options 'modules #f))
           (fsm     (puml->fsm (current-input-port)
                               #:module      (puml-modules modules)
                               #:debug-mode? debug-mode?)))
      (when (option-ref options 'validate #f)
        (let ((validation-result (fsm-validate fsm)))
          (unless (null? validation-result)
            (pretty-print validation-result (current-error-port))
            (exit 1))))
      (cond
       ((option-ref options 'print-transition-table #f)
        (pretty-print-transition-table fsm))
       (else
        (let ((name   (option-ref options 'fsm-name 'custom-fsm))
              (module (option-ref options 'fsm-module #f)))
          (fsm-compile fsm
                       #:fsm-name      name
                       #:fsm-module    (and module
                                            (eval-string/quote module))
                       #:extra-modules (and modules
                                            (eval-string/quote modules)))))))))

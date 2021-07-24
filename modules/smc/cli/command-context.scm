(define-module (smc cli command-context)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (smc core log)
  #:use-module (smc compiler)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc cli common)
  #:export (command-context))

(define (print-help)
  (display "\
Usage: smc context [options]

Analyze or generate a context stub based on a given PlantUML file.

Options:
  --help, -h        Print this message and exit.
  --resolve, -r     Show resolved and unresolved procedures.
  --generate, -g    Generate a context stub from a given PlantUML file.
                    (Not implemented yet.)
  --debug           Enable the debug mode.

"))


(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (load-path                (single-char #\L) (value #t))
    (modules                  (single-char #\U) (value #t))
    (resolve                  (single-char #\r) (value #f))
    ;; (generate                 (single-char #\g) (value #f))
    (debug                                      (value #f))))

(define (command-context args)
  (let* ((options          (getopt-long args %option-spec))
         (extra-load-paths (option-ref options 'load-path ""))
         (modules          (option-ref options 'modules   #f))
         (resolve?         (option-ref options 'resolve   #f))
         (generate?        (option-ref options 'generate  #f))
         (debug-mode?      (option-ref options 'debug     #f)))

    (when (option-ref options 'help #f)
      (print-help)
      (exit 0))

    (log-use-stderr! debug-mode?)

    (add-to-load-path* (string-split extra-load-paths #\:))

    (cond
     (resolve?
      (let ((fsm (puml->fsm (current-input-port)
                            #:module      (puml-modules modules)
                            #:keep-going? #t
                            #:debug-mode? debug-mode?)))
        (puml-context-print-resolver-status (fsm-parent-context fsm)
                                            (current-output-port)))))))

;;; command-context.scm ends here.


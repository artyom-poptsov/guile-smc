(define-module (smc cli command-context)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (smc core log)
  #:use-module (smc core set)
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
  --generate-module, -G <module>
                    Generate a context module stub from a given PlantUML file.
  --debug           Enable the debug mode.

"))



(define (%write-separator port)
  (display #\ff port)
  (newline port))

(define %write-commentary
  (case-lambda
    ((port comment)
     (format port ";; ~a~%" comment))
    ((port . comments)
     (for-each (lambda (line)
                 (format port ";; ~a~%" line))
               comments))))

(define (%generate-context-module module exports port)
  (pretty-print `(define-module ,(eval-string
                                  (string-append "(quote " module ")"))
                   #:export ,exports)
                port)
  (newline port))

(define (%generate-context-action-procedure name port)
  (pretty-print `(define (,name ctx event)
                   ctx)
                port)
  (newline port))

(define (%generate-context-guard-procedure name port)
  (pretty-print `(define (,name ctx event)
                   #t)
                port)
  (newline port))

(define (generate-context module procedures port)
  (when module
    (%generate-context-module module procedures port))

  (newline port)
  (%write-separator port)
  (%write-commentary port "Guards.")
  (for-each (lambda (proc)
              (%generate-context-guard-procedure proc port))
            (filter (lambda (proc)
                      (string-prefix? "guard:" (symbol->string proc)))
                    procedures))

  (newline port)
  (%write-separator port)
  (%write-commentary port "Actions.")
  (for-each (lambda (proc)
              (%generate-context-action-procedure proc port))
            (filter (lambda (proc)
                      (string-prefix? "action:" (symbol->string proc)))
                    procedures)))




(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (load-path                (single-char #\L) (value #t))
    (modules                  (single-char #\U) (value #t))
    (resolve                  (single-char #\r) (value #f))
    (generate                 (single-char #\g) (value #f))
    (generate-module          (single-char #\G) (value #t))
    (debug                                      (value #f))))

(define (command-context args)
  (let* ((options          (getopt-long args %option-spec))
         (extra-load-paths (option-ref options 'load-path ""))
         (modules          (option-ref options 'modules   #f))
         (resolve?         (option-ref options 'resolve   #f))
         (generate?        (option-ref options 'generate  #f))
         (generate-module  (option-ref options 'generate-module #f))
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
                                            (current-output-port))))
     (generate?
      (let ((fsm (puml->fsm (current-input-port)
                            #:module      (puml-modules modules)
                            #:keep-going? #t
                            #:debug-mode? debug-mode?)))
        (generate-context #f
                          (set-content (puml-context-unresolved-procedures
                                        (fsm-parent-context fsm)))
                          (current-output-port))))
     (generate-module
      (let ((fsm (puml->fsm (current-input-port)
                            #:module      (puml-modules modules)
                            #:keep-going? #t
                            #:debug-mode? debug-mode?)))
        (generate-context generate-module
                          (set-content (puml-context-unresolved-procedures
                                        (fsm-parent-context fsm)))
                          (current-output-port)))))))

;;; command-context.scm ends here.


(define-module (smc cli command-context)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (smc core log)
  #:use-module (smc core set)
  #:use-module (smc compiler)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc cli common)
  #:use-module (smc config)
  #:use-module (smc compiler guile-common)
  #:use-module (smc compiler guile-standalone)
  #:export (command-context))

(define (print-help)
  (display "\
Usage: smc context [options] [input-file]

Analyze or generate a context stub based on a given PlantUML file.  If
INPUT-FILE is not provided, than the command will read the standard input.

Options:
  --help, -h        Print this message and exit.
  --resolve, -r     Show resolved and unresolved procedures.
  --standalone, -s
                    Generate a standalone compiler context.
  --generate, -g    Generate a context stub from a given PlantUML file.
  --module, -m <module>
                    Place the output code into a specified module
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

(define* (%generate-context-module module exports port)
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
  "Generate a context stub that can be used as the foundation for user
context."
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



(define (get-module-exports defmod)
  "Get all the exports from a module definition DEFMOD as a list."
  (if (equal? (car defmod) '#:export)
      (cadr defmod)
      (get-module-exports (cdr defmod))))

(define (generate-smc-context module output-port)
  "Generate a Guile-SMC context that can server as intermediate module for the
derivative contexts.  The context is placed into a MODULE and printed to a
specified OUTPUT-PORT."
  (let* ((context-code (fsm-get-context-code %guile-smc-modules-directory
                                             #:skip-define-module? #f))
         (exports      (fold (lambda (sexp prev)
                               (cond
                                ((equal? (car sexp) 'define-module)
                                 (append (get-module-exports sexp) prev))
                                ((equal? (car sexp) 'define-public)
                                 (append (list (caadr sexp)) prev))
                                ((equal? (car sexp) 'make-char-guard)
                                 (append (list (cadr sexp)) prev))
                                (else
                                 prev)))
                             '()
                             context-code)))
    (write-header output-port)
    (newline)
    (pretty-print `(define-module ,(eval-string
                                    (string-append "(quote " module ")"))
                     #:use-module (smc core common)
                     #:use-module (smc context context)
                     #:use-module (smc context char-context)
                     #:use-module (smc core log)
                     #:re-export ,exports))))

(define* (generate-smc-context/standalone module
                                          output-port
                                          #:key
                                          (optimize? #f))
  "Generate a Guile-SMC context that contains everything that is needed for
derivative contexts.  The context is placed into a MODULE and printed to a
specified OUTPUT-PORT."
  (write-header output-port)
  (newline)
  (let* ((context-code (fsm-get-context-code %guile-smc-modules-directory
                                             #:skip-define-module? #f))
         (exports      (fold (lambda (sexp prev)
                              (cond
                               ((equal? (car sexp) 'define-module)
                                (append (get-module-exports sexp) prev))
                               (else
                                prev)))
                             '()
                             context-code)))

    (pretty-print `(define-module ,(eval-string
                                    (string-append "(quote " module ")"))
                     #:use-module (oop goops)
                     #:use-module (logging logger)
                     #:use-module (logging rotating-log)
                     #:use-module (scheme documentation)
                     #:use-module (ice-9 textual-ports)
                     #:use-module (ice-9 format)
                     #:export     ,exports))
    (newline output-port)
    (form-feed output-port)

    (for-each (lambda (sexp)
                (unless (equal? (car sexp) 'define-module)
                  (pretty-print sexp output-port #:display? #f)
                  (newline)))
              (if optimize?
                  (prune-unused-definitions
                   (prune-unused-definitions context-code (list fsm-code))
                   (list fsm-code))
                  context-code))

    (newline output-port)))


(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (load-path                (single-char #\L) (value #t))
    (standalone               (single-char #\s) (value #f))
    (use-modules              (single-char #\U) (value #t))
    (resolve                  (single-char #\r) (value #f))
    (generate                 (single-char #\g) (value #f))
    (module                   (single-char #\m) (value #t))
    (debug                                      (value #f))))

(define (command-context args)
  (let* ((options          (getopt-long args %option-spec))
         (extra-load-paths (option-ref options 'load-path ""))
         (extra-modules    (option-ref options 'use-modules #f))
         (module           (option-ref options 'module    #f))
         (resolve?         (option-ref options 'resolve   #f))
         (generate?        (option-ref options 'generate  #f))
         (standalone?      (option-ref options 'standalone #f))
         (debug-mode?      (option-ref options 'debug     #f))
         (args             (option-ref options '()        #f)))

    (when (option-ref options 'help #f)
      (print-help)
      (exit 0))

    (log-use-stderr! debug-mode?)

    (add-to-load-path* (string-split extra-load-paths #\:))

    (let ((port (if (null? args)
                    (current-input-port)
                    (let ((p (open-input-file (car args))))
                      (unless p
                        (error "Could not open a file" (car args)))
                      p))))

      (cond
       (resolve?
        (let ((fsm (puml->fsm port
                              #:module      (puml-modules extra-modules)
                              #:keep-going? #t
                              #:debug-mode? debug-mode?)))
          (puml-context-print-resolver-status (fsm-parent-context fsm)
                                              (current-output-port))))
       (generate?
        (let ((fsm (puml->fsm port
                              #:module      (puml-modules extra-modules)
                              #:keep-going? #t
                              #:debug-mode? debug-mode?)))
          (generate-context module
                            (set-content (puml-context-unresolved-procedures
                                          (fsm-parent-context fsm)))
                            (current-output-port))))
       (else
        (if standalone?
            (generate-smc-context/standalone module (current-output-port))
            (generate-smc-context module (current-output-port))))))))

;;; command-context.scm ends here.


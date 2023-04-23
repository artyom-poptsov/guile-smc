;;; command-profile.scm -- Guile-SMC 'smc profile' command.

;; Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Implementation of 'smc profile' command.


;;; Code:


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
  #:export (command-context
            cli-context-write-commentary))

(define (print-help)
  (display (string-append "\
Usage: smc context [options] [input-file]

Analyze or generate a context stub based on a given PlantUML file.  If
INPUT-FILE is not provided, than the command will read the standard input.

Options:
  --help, -h        Print this message and exit.
  --resolve, -r     Show resolved and unresolved procedures.
  --standalone, -s
                    Generate a standalone compiler context.
  --type, -T <type> Set the context type for the output context.
  --generate, -g    Generate a context stub from a given PlantUML file.
  --core-modules-path <path>
                    Set the path where Guile-SMC core modules are stored.

                    Default value:
                      " %guile-smc-modules-directory "
  --module, -m <module>
                    Place the output code into a specified module
  --load-path, -L <load-path>
                    Add an extra load path.
  --log-driver <driver>
                    Set the log driver.
                    Supported values:
                    - \"syslog\" -- use syslog as the logging driver.
                    - \"file\" -- log to a specified file. Output files are
                      rotated as needed.
                      Options:
                      \"file\" -- the full path to the log file.
                    - \"null\" -- disable logging (discard all the messages.)

                    Default value is \"syslog\"
  --log-opt <options>
                    Set the logging options.  The set of options depends on
                    the logging driver.
                    Format:
                      \"key1=value1,key2=value2\"
                    Example:
                      \"file=/tmp/smc.log\"
  --debug           Enable the debug mode.

")))



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
  (form-feed port)
  (cli-write-commentary port "Guards.")
  (for-each (lambda (proc)
              (%generate-context-guard-procedure proc port))
            (filter (lambda (proc)
                      (string-prefix? "guard:" (symbol->string proc)))
                    procedures))

  (newline port)
  (form-feed port)
  (cli-write-commentary port "Actions.")
  (for-each (lambda (proc)
              (%generate-context-action-procedure proc port))
            (filter (lambda (proc)
                      (string-prefix? "action:" (symbol->string proc)))
                    procedures)))



(define (get-module-exports defmod)
  "Get all the exports from a module definition DEFMOD as a list."
  (if (null? defmod)
      '()
      (if (equal? (car defmod) '#:export)
          (cadr defmod)
          (get-module-exports (cdr defmod)))))

(define* (generate-smc-context core-modules-path
                               module
                               output-port
                               #:key
                               (type #f))
  "Generate a Guile-SMC context that can server as intermediate module for the
derivative contexts.  The context is placed into a MODULE and printed to a
specified OUTPUT-PORT."
  (let* ((context-code (fsm-get-context-code core-modules-path
                                             #:type type
                                             #:skip-define-module? #f))
         (exports      (fold (lambda (sexp prev)
                               (cond
                                ((equal? (car sexp) 'define-module)
                                 (append (get-module-exports sexp) prev))
                                ((equal? (car sexp) 'define-public)
                                 (append (list (caadr sexp)) prev))
                                ((or (equal? (car sexp) 'char:make-guard)
                                     (equal? (car sexp) 'char:make-charset-guard)
                                     (equal? (car sexp) 'u8:make-char-guard)
                                     (equal? (car sexp) 'u8:make-charset-guard))
                                 (append (list (cadr sexp)) prev))
                                (else
                                 prev)))
                             '()
                             context-code)))
    (write-header output-port)
    (newline)
    (let ((common-context-code
           `(define-module ,(eval-string
                             (string-append "(quote " module ")"))
              #:use-module (smc core common)
              #:use-module (smc context common)
              #:use-module (smc context u8)
              #:use-module (smc context char)
              #:use-module (smc core config)
              #:use-module (smc core log)))
          (re-exports-list `(#:re-export ,exports)))
      (pretty-print
       (case type
         ((oop)
          `(,@common-context-code
            ,@'(#:use-module (smc context oop generic)
                #:use-module (smc context oop port)
                #:use-module (smc context oop char)
                #:use-module (smc context oop u8))
            ,@re-exports-list))
         ((functional)
          `(,@common-context-code
            ,@'(#:use-module (smc context functional generic)
                #:use-module (smc context functional char)
                #:use-module (smc context functional u8))
            ,@re-exports-list))
         (else
          `(,@common-context-code
            ,@re-exports-list)))))))

(define* (generate-smc-context/standalone core-modules-path
                                          module
                                          output-port
                                          #:key
                                          (type #f))
  "Generate a Guile-SMC context that contains everything that is needed for
derivative contexts.  The context is placed into a MODULE and printed to a
specified OUTPUT-PORT."
  (write-header output-port)
  (newline)
  (log-debug "generate-smc-context/standalone: Core modules path: ~a"
             core-modules-path)
  (let* ((context-code (fsm-get-context-code core-modules-path
                                             #:type type
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

    (for-each (lambda (sexp)
                (if (equal? (car sexp) 'define-module)
                    (write-section-header (format #f "Code from ~a" (cadr sexp))
                                          output-port)
                    (pretty-print sexp output-port #:display? #f))
                (newline output-port))
              context-code)

    (write-footer "Guile-SMC context" output-port)))


(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (load-path                (single-char #\L) (value #t))
    (standalone               (single-char #\s) (value #f))
    (use-modules              (single-char #\U) (value #t))
    (type                     (single-char #\T) (value #t))
    (resolve                  (single-char #\r) (value #f))
    (generate                 (single-char #\g) (value #f))
    (core-modules-path                          (value #t))
    (module                   (single-char #\m) (value #t))
    (log-driver                                 (value #t))
    (log-opt                                    (value #t))
    (debug                                      (value #f))))

(define (command-context args)
  (let* ((options          (getopt-long args %option-spec))
         (extra-load-paths (option-ref options 'load-path ""))
         (type             (option-ref options 'type #f))
         (core-modules-path (option-ref options
                                        'core-modules-path
                                        %guile-smc-modules-directory))
         (extra-modules    (option-ref options 'use-modules #f))
         (module           (option-ref options 'module    "(context)"))
         (resolve?         (option-ref options 'resolve   #f))
         (generate?        (option-ref options 'generate  #f))
         (standalone?      (option-ref options 'standalone #f))
         (log-driver       (option-ref options 'log-driver "syslog"))
         (log-opt          (cli-options->alist
                            (option-ref options 'log-opt "")))
         (debug-mode?      (option-ref options 'debug     #f))
         (help-needed?     (option-ref options 'help      #f))
         (args             (option-ref options '()        #f)))

    (when (option-ref options 'help #f)
      (print-help)
      (exit 0))

    (when debug-mode?
      (log-use-stderr! debug-mode?))

    (smc-log-init! log-driver log-opt)

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
            (generate-smc-context/standalone core-modules-path
                                             module
                                             (current-output-port)
                                             #:type (and (string? type)
                                                         (string->symbol type)))
            (generate-smc-context core-modules-path
                                  module
                                  (current-output-port)
                                  #:type (and (string? type)
                                              (string->symbol type)))))))))

;;; command-context.scm ends here.


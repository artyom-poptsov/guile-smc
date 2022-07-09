;;; command-run.scm -- Guile-SMC 'smc run' command.

;; Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Implementation of 'smc run' command.  This command allows to run a state
;; machine without the need of explicit intermediate steps.


;;; Code:

(define-module (smc cli command-run)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 r5rs)
  #:use-module (oop goops)
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
  --log-file <file> Log file to use.  Pass \"-\" as the file to use the standard
                    error stream (stderr.)
                    'smc run' logs to syslog by default.
  --debug           Enable the debug mode.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (eval                     (single-char #\e) (value #t))
    (load-path                (single-char #\L) (value #t))
    (context                  (single-char #\C) (value #t))
    (modules                  (single-char #\U) (value #t))
    (validate                                   (value #f))
    (log-file                                   (value #t))
    (debug                                      (value #f))))



(define %default-result-handler
  "(lambda (context) (display context) (newline))")

(define %default-context-thunk
  "(lambda () #f)")


(define (configure-logging! log-file)
  "Configure Guile-SMC logging to use a LOG-FILE."
  (log-clear-handlers!)
  (log-add-handler! (make <port-log/us>
                      #:port (if (string=? log-file "-")
                                 (current-error-port)
                                 (open-output-file log-file)))))



(define (command-run args)
  "Handle the 'run' CLI command with the given ARGS."
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
         (log-file         (option-ref options 'log-file  #f))
         (args             (option-ref options '()        #f)))

    (when (or (option-ref options 'help #f) (null? args))
      (print-help)
      (exit 0))

    (log-use-stderr! debug-mode?)
    (add-to-load-path* (string-split extra-load-paths #\:))

    (when log-file
      (configure-logging! log-file))

    (let* ((puml-file (car args))
           (port    (open-input-file puml-file))
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
            (fsm-debug-mode-set! fsm ,debug-mode?)
            (let ((proc   (eval-string ,context-thunk)))
              (unless (thunk? proc)
                (error "context-thunk must be a procedure with zero parameters."
                       ,context-thunk))
              (let ((context (fsm-run! fsm (proc))))
                (when ,debug-mode?
                  (let ((bname ,(basename puml-file)))
                    (display ";;;\n" (current-error-port))
                    (format (current-error-port)
                            ";;; [~a] Statistics:\n"
                            bname)
                    (for-each (lambda (record)
                                (format (current-error-port)
                                        ";;; [~a]   ~20,,a ~10,,@a~%"
                                        bname
                                        (format #f "~a:" (car record))
                                        (cdr record)))
                              (fsm-statistics fsm))))
                ((eval-string ,eval-proc) context))))
         env)))))

;;; command-run.scm ends here.

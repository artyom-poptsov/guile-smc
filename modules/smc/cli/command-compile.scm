;;; command-compile.scm -- Guile-SMC 'smc compile' command.

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

;; Implementation of 'smc compile' command.


;;; Code:


(define-module (smc cli command-compile)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (smc core log)
  #:use-module (smc compiler)
  #:use-module (smc compiler guile)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc cli common)
  #:export (command-compile))


(define (print-compile-help)
  (display "\
Usage: smc compile [options] [input-file]

The program reads a PlantUML transition diagram from an INPUT-FILE or the
standard input if no file specified, and creates a finite-state machine (FSM)
from the formal description.

Then the FSM can be validated and/or compiled.

If no INPUT-FILE is specified, then the input PlantUML transition diagram is
read from the standard input.

Options:
  --help, -h        Print this message and exit.
  --print-transition-table, -p
                    Print the FSM transition table to the standard
                    output.
  --load-path, -L <paths>
                    Add a paths separated by a colon to load paths.
  --modules, -U <extra-modules>
                    Load additional modules.  The value must be the same
                    as for 'use-modules'.  Example value:
                      \"((smc context char-context) (smc puml-context))\"
  --fsm-name, -n <name>
                    Set the name for the output FSM.
  --fsm-module, -m <module>
                    Set the module for the output FSM.  Example value:
                      \"(smc puml-fsm)\"
  --validate        Validate the output FSM and print the validation result.
                    The exit code is 0 if the validation is passed,
                    and a non-zero value otherwise.
  --target, -t <target>
                    Compilation target.  Allowed values:
                      \"guile\", \"guile-standalone\", \"guile-standalone-copy\"
                    Default value is \"guile\".
  --debug           Enable the debug mode.

"))


(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (print-transition-table   (single-char #\p) (value #f))
    (load-path                (single-char #\L) (value #t))
    (modules                  (single-char #\U) (value #t))
    (fsm-name                 (single-char #\n) (value #t))
    (fsm-module               (single-char #\m) (value #t))
    (target                   (single-char #\t) (value #t))
    (validate                                   (value #f))
    (debug                                      (value #f))))

(define (command-compile args)
  (let* ((options          (getopt-long args %option-spec))
         (extra-load-paths (option-ref options 'load-path  ""))
         (module           (option-ref options 'fsm-module #f))
         (extra-modules    (option-ref options 'modules    #f))
         (target           (option-ref options 'target     "guile"))
         (debug-mode?      (option-ref options 'debug      #f))
         (fsm-module        (and module
                                 (eval-string/quote module)))
         (fsm-extra-modules (and extra-modules
                                 (eval-string/quote extra-modules)))
         (fsm-extra-modules-rewritten
          (if (string=? target "guile-standalone-copy")
              (map (lambda (m) (cons (car fsm-module) m))
                   fsm-extra-modules)
              fsm-extra-modules))
         (args             (option-ref options '()        #f)))

    (when (option-ref options 'help #f)
      (print-compile-help)
      (exit 0))

    (log-use-stderr! debug-mode?)

    (add-to-load-path* (string-split extra-load-paths #\:))

    (log-debug "Target: ~a" target)
    (when (string=? target "guile-standalone-copy")
      (copy-dependencies "." fsm-module fsm-extra-modules))

    (log-debug "arguments: ~a" args)
    (let* ((port (if (null? args)
                     (current-input-port)
                     (let ((p (open-input-file (car args))))
                       (unless p
                         (error "Could not open a file" (car args)))
                       p)))
           (fsm (puml->fsm port
                           #:module      (puml-modules extra-modules)
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
        (let ((name (option-ref options 'fsm-name 'custom-fsm)))
          (fsm-compile fsm
                       #:fsm-name      name
                       #:fsm-module    fsm-module
                       #:extra-modules fsm-extra-modules-rewritten
                       #:target        (string->symbol target))))))))

;;; command-compile.scm ends here.

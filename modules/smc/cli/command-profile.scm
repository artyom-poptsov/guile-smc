;;; command-profile.scm -- Guile-SMC 'smc profile' command.

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

;; Implementation of 'smc profile' command.


;;; Code:

(define-module (smc cli command-profile)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 r5rs)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc core set)
  #:use-module (smc compiler)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc trace-context)
  #:use-module (smc trace-fsm)
  #:use-module (smc cli common)
  #:export (command-profile))



(define (print-help)
  (display "\
Usage: smc profile [options] <log-file>

Profile a state machine using a logged execution trace.

Options:
  --help, -h        Print this message and exit.
  --log-driver <driver>
                    Set the log driver.
                    Supported values:
                    - \"syslog\" -- use syslog as the logging driver.
                    - \"file\" -- log to a specified file. Output files are
                      rotated as needed.
                      Options:
                      \"file\" -- the full path to the log file.
                    - \"none\" -- disable logging (discard all the messages.)

                    Default value is \"syslog\"
  --log-opt <options>
                    Set the logging options.  The set of options depends on
                    the logging driver.
                    Format:
                      \"key1=value1,key2=value2\"
                    Example:
                      \"file=/tmp/smc.log\"
  --debug           Enable the debug mode.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (log-driver                                 (value #t))
    (log-opt                                    (value #t))
    (debug                                      (value #f))))

(define-method (trace-profile-time (trace <list>))
  (let loop ((prev-tr    #f)
             (trs        trace)
             (result     (make-hash-table)))
    (if (null? trs)
        result
        (let ((tr (car trs)))
          (if (log-entry:message? tr)
              (loop prev-tr (cdr trs) result)
              (if prev-tr
                  (let* ((prev-state (log-entry-transition-to prev-tr))
                         (time       (hash-ref result prev-state)))
                    (begin
                      (hash-set! result
                                 prev-state
                                 (+ (if time
                                        time
                                        0)
                                    (- (log-entry-timestamp-usec tr)
                                       (log-entry-timestamp-usec prev-tr))))
                      (loop tr (cdr trs) result)))
                  (begin
                    (loop tr (cdr trs) result))))))))



(define (command-profile args)
  (let* ((options          (getopt-long args %option-spec))
         (debug-mode?      (option-ref options 'debug     #f))
         (log-driver       (option-ref options 'log-driver "syslog"))
         (log-opt          (cli-options->alist
                            (option-ref options 'log-opt "")))
         (args             (option-ref options '()        #f)))

    (when (or (option-ref options 'help #f) (null? args))
      (print-help)
      (exit 0))

    (when debug-mode?
      (log-use-stderr! debug-mode?))

    (smc-log-init! log-driver log-opt)

    (let* ((port    (open-input-file (car args)))
           (context (fsm-run! (make <trace-fsm>
                                #:debug-mode? debug-mode?)
                              (make <trace-context>
                                #:port port)))
           (trace   (reverse (trace-context-result context)))
           (total-time (- (log-entry-timestamp-usec (car (trace-context-result context)))
                          (log-entry-timestamp-usec (car trace)))))
      (format #t "Total transitions: ~a~%" (length trace))
      (format #t "Total time:        ~a us~%" total-time)
      (display "Stats:\n")
      (for-each (lambda (kv)
                  (unless (string=? (car kv) "*")
                    (format #t
                            "  ~a: ~a us (~2,4f %)~%"
                            (car kv)
                            (cdr kv)
                            (* (/ (cdr kv) total-time) 100))))
                (sort (hash-map->list cons (trace-profile-time trace))
                      (lambda (kv1 kv2)
                        (> (cdr kv1) (cdr kv2))))))))

;;; command-profile.scm ends here.

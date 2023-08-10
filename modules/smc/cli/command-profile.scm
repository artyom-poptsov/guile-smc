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
  #:export (command-profile

            print-human-readable))



(define (print-help)
  (display "\
Usage: smc profile [options] [log-file]

Profile a state machine using a logged execution trace.  If no log file is
specified, then the standard input will be read.

Options:
  --help, -h        Print this message and exit.
  --format, -f <output-format>
                    Output format.  Supported values:
                    - \"human-readable\" (default)
                    - \"json\"
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

                    There's an option \"stderr\" that is handled independently
                    from the log driver that allows to configure stderr logging.
                    Example values:
                      \"stderr=true\"
                      \"stderr=false\"

  --debug           Enable the debug mode.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (format                   (single-char #\f) (value #t))
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
                    (let ((new-time (+ (if time
                                           time
                                           0)
                                       (- (log-entry-timestamp-usec tr)
                                          (log-entry-timestamp-usec prev-tr)))))
                      (when (negative? new-time)
                        (error "Time cannot be negative"
                               `((new-time       . ,new-time)
                                 (current-entry  . ,tr)
                                 (previous-entry . ,prev-tr))))
                      (hash-set! result
                                 prev-state
                                 new-time)
                      (loop tr (cdr trs) result)))
                  (begin
                    (loop tr (cdr trs) result))))))))



(define (print-human-readable trace total-time)
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
                    (> (cdr kv1) (cdr kv2))))))

(define (print-json trace total-time)
  (display "{")
  (format #t "\"Total transitions\": ~a," (length trace))
  (format #t "\"Total time (us)\":~a," total-time)
  (display "\"Stats\": {")
  (let* ((data (sort (hash-map->list cons (trace-profile-time trace))
                     (lambda (kv1 kv2)
                       (> (cdr kv1) (cdr kv2)))))
         (last-rec (car (reverse data))))
    (for-each (lambda (kv)
                (unless (string=? (car kv) "*")
                  (format #t
                          "\"~a\": {\"Time (us)\": ~a, \"Percent\": ~2,4f}"
                          (car kv)
                          (cdr kv)
                          (* (/ (cdr kv) total-time) 100))
                  (if (equal? kv last-rec)
                      (display " ")
                      (display ", "))))
              data))
  (display "}}\n"))

(define (command-profile args)
  (let* ((options          (getopt-long args %option-spec))
         (debug-mode?      (option-ref options 'debug     #f))
         (output-format    (option-ref options 'format    "human-readable"))
         (log-driver       (option-ref options 'log-driver "syslog"))
         (log-opt          (cli-options->alist
                            (option-ref options 'log-opt "")))
         (args             (option-ref options '()        #f)))

    (when (option-ref options 'help #f)
      (print-help)
      (exit 0))

    (smc-log-init! log-driver log-opt)

    (let* ((port    (if (null? args)
                        (current-input-port)
                        (let ((p (open-input-file (car args))))
                          (unless p
                            (error "Could not open a file" (car args)))
                          p)))
           (context (fsm-run! (make <trace-fsm>
                                #:debug-mode? debug-mode?)
                              (make <trace-context>
                                #:port port)))
           (trace   (reverse (trace-context-result context)))
           (total-time (- (log-entry-timestamp-usec (car (trace-context-result context)))
                          (log-entry-timestamp-usec (car trace)))))
      (when (negative? total-time)
        (error "Total time cannot be negative"
               `((total-time . ,total-time)
                 (first-entry . ,(car trace))
                 (last-entry . ,(car (trace-context-result context))))))
      (cond
       ((string=? output-format "human-readable")
        (print-human-readable trace total-time))
       ((string=? output-format "json")
        (print-json trace total-time))
       (#t
        (error "Unknown output format" output-format))))))

;;; command-profile.scm ends here.

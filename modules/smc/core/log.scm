;;; log.scm -- Guile-SMC logging facilities.

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

;; This module contains Guile-SMC logging facilities.


;;; Code:

(define-module (smc core log)
  #:use-module (scheme documentation)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 format)
  #:use-module (logging logger)
  #:use-module (oop goops)
  #:use-module (smc core config)
  #:use-module (smc core common)
  #:use-module (smc core state)
  #:export (<precise-logger>
            <system-log>
            <null-log>
            <precise-port-log>
            precise-logger?
            precise-port-log?
            syslog-handler?
            smc-log-init!
            smc-log
            log-add-handler!
            log-clear-handlers!
            log-error
            log-warning
            log-info
            log-debug
            log-use-stderr!

            ;; Helper procedures.
            %precise-log-formatter
            %precise-log-helper))


;;; Constants.

(define-with-docs %default-port-log-file
  "The full path to the default log file used with <precise-port-log>."
  "/var/log/smc.log")

(define-with-docs %default-guile-smc-syslog-tag
  "The default syslog tag used for Guile-SMC."
  "guile-smc")


;;; Precise Logger.

(define-class-with-docs <precise-logger> (<logger>)
  "Guile-SMC precise logger that prints log with microsecond accuracy.")

(define (precise-logger? x)
  "Check if X is a <precise-logger> instance."
  (is-a? x <precise-logger>))

(define (level-enabled? lgr lvl)
  "Check if a log level LVL is enabled for a logger LGR."
  (hashq-ref (slot-ref lgr 'levels) lvl #t))

(define-method (%precise-log-helper (self <precise-logger>) level objs)
  (when (level-enabled? self level)
    (let ((cur-time (gettimeofday)))
      (for-each (lambda (str)
                  (unless (string-null? str)
                    ;; pass the string to each log handler for SELF.
                    (for-each (lambda (handler)
                                (accept-log handler level cur-time str))
                              (slot-ref self 'log-handlers))))
                ;; split the string at newlines into different log statements
                (string-split
                 (with-output-to-string (lambda () (for-each (lambda (o) (display o)) objs)))
                 #\nl)))))

(define-method (log-msg (lgr <precise-logger>) lvl . objs)
  (%precise-log-helper lgr lvl objs))


;;; null log handler.

(define-class-with-docs <null-log> (<log-handler>)
  "A log handler that discards all the log messages.")

(define-method-with-docs (accept-log (log <null-log>) level time str)
  "This method discards all the parameters and always return #t."
  #t)


;;; syslog handler.

(define-class-with-docs <system-log> (<log-handler>)
  "This is a log handler which writes logs to the syslog."

  ;; Syslog logger console command.
  ;;
  ;; <string>
  (logger
   #:init-value %logger-binary
   #:getter     syslog-handler-logger)

  ;; syslog process tag that will be logged.
  ;;
  ;; <string>
  (tag
   #:init-keyword #:tag
   #:getter       syslog-handler-tag)

  ;; Should the logger log to stderr?
  ;;
  ;; <boolean>
  (use-stderr?
   #:init-value   #f
   #:init-keyword #:use-stderr?
   #:getter       syslog-handler-use-stderr?
   #:setter       syslog-handler-use-stderr!))

(define (syslog-handler? x)
  (is-a? x <system-log>))

(define-method (accept-log (log <system-log>) level time str)
  (let* ((command (format #f "~a ~a -p 'user.~a' -t '~a' '~a'"
                          (syslog-handler-logger log)
                          (if (syslog-handler-use-stderr? log)
                              "-s"
                              "")
                          level
                          (syslog-handler-tag log)
                          str))
         (result (system command)))
    (unless (zero? result)
      (error "Could not log a message"))))


;;; The precise logger API is inspired by (logging logger) API.

(define-class-with-docs <precise-port-log> (<log-handler>)
  "Microsecond version of <port-log> from (logging port-log)."

  ;; <port>
  (port
   #:init-keyword #:port
   #:init-value   #f
   #:accessor     port))

(define (precise-port-log? x)
  (is-a? x <precise-port-log>))

(define (%precise-log-formatter lvl time str)
  (with-output-to-string
    (lambda ()
      (display (strftime "%F %H:%M:%S" (localtime (car time))))
      (display ".")
      (format #t "~6,'0d" (cdr time))
      (display " (")
      (display (symbol->string lvl))
      (display "): ")
      (display str)
      (newline))))

(define-method (initialize (self <precise-port-log>) args)
  (next-method)
  (slot-set! self 'formatter %precise-log-formatter))

(define-method (emit-log (self <precise-port-log>) str)
  (display str (port self)))

(define-method (flush-log (self <precise-port-log>))
  (force-output (port self)))

(define-method (close-log! (self <precise-port-log>))
  (close-port (port self))
  (set! (port self) (%make-void-port "w")))



(define-with-docs %logger
  "Default logger instance for Guile-SMC."
  (make <precise-logger>))

(set-default-logger! %logger)
(open-log! %logger)

(define-method (log-add-handler! (handler <log-handler>))
  (add-handler! %logger handler))

(define-method (log-clear-handlers!)
  (slot-set! %logger 'log-handlers '()))

(define-method (smc-log-init! (driver <string>) (options <list>))
  (log-clear-handlers!)
  (cond
   ((string=? driver "syslog")
    (log-add-handler! (make <system-log> #:tag %default-guile-smc-syslog-tag)))
   ((string=? driver "file")
    (let* ((file (or (assoc-ref options 'file)
                     %default-port-log-file))
           (port (open-output-file file)))
      (log-add-handler! (make <precise-port-log> #:port port))))
   ((string=? driver "null")
    (log-add-handler! (make <null-log>)))
   (else
    (error "Unknown log driver" driver options))))


;; Initialize the default logger.

(smc-log-init! "syslog" '())


(define-method-with-docs (log-use-stderr! (value <boolean>))
  "Enable or disable logging to stderr."
  #f)
  ;; (syslog-handler-use-stderr! %syslog value))

(define (smc-log level fmt . args)
  (let ((message (apply format #f fmt args)))
    (log-msg %logger level message)))



(define (log-error fmt . args)
  "Log a formatted error message."
  (apply smc-log 'ERROR fmt args))

(define (log-warning fmt . args)
  "Log a formatted warning message."
  (apply smc-log 'WARNING fmt args))

(define (log-info fmt . args)
  "Log a formatted informational message."
  (apply smc-log 'INFO fmt args))

(define (log-debug fmt . args)
  "Log a formatted debug message."
  (apply smc-log 'DEBUG fmt args))

;;; log.scm ends here.

;;; log.scm -- Guile-SMC logging facilities.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (logging logger)
  #:use-module (oop goops)
  #:use-module (smc core common)
  #:use-module (smc core state)
  #:export (log
            log-error
            log-warning
            log-info
            log-debug
            log-use-stderr!))



(define-class-with-docs <syslog> (<log-handler>)
  "This is a log handler which writes logs to the syslog."

  ;; Syslog logger console command.
  ;;
  ;; <string>
  (logger
   #:init-value "logger"
   #:getter     syslog-logger)

  ;; syslog process tag that will be logged.
  ;;
  ;; <string>
  (tag
   #:init-keyword #:tag
   #:getter       syslog-tag)

  ;; Should the logger log to stderr?
  ;;
  ;; <boolean>
  (use-stderr?
   #:init-value   #f
   #:init-keyword #:use-stderr?
   #:getter       syslog-use-stderr?
   #:setter       syslog-use-stderr!))



(define-method (accept-log (log <syslog>) level time str)
  (let* ((command (format #f "~a ~a -p 'user.~a' -t '~a' '~a'"
                          (syslog-logger log)
                          (if (syslog-use-stderr? log)
                              "-s"
                              "")
                          level
                          (syslog-tag log)
                          str))
         (result (system command)))
    (unless (zero? result)
      (error "Could not log a message"))))



(define-with-docs %syslog
  "Default syslog handler instance for Guile-SMC."
  (make <syslog> #:tag "guile-smc"))

(define-with-docs %logger
  "Default logger instance for Guile-SMC."
  (make <logger>))

(add-handler! %logger %syslog)
(set-default-logger! %logger)
(open-log! %logger)


(define-method-with-docs (log-use-stderr! (value <boolean>))
  "Enable or disable logging to stderr."
  (syslog-use-stderr! %syslog value))

(define (log level fmt . args)
  (let ((message (apply format #f fmt args)))
    (log-msg level message)))



(define (log-error fmt . args)
  "Log a formatted error message."
  (apply log 'ERROR fmt args))

(define (log-warning fmt . args)
  "Log a formatted warning message."
  (apply log 'WARNING fmt args))

(define (log-info fmt . args)
  "Log a formatted informational message."
  (apply log 'INFO fmt args))

(define (log-debug fmt . args)
  "Log a formatted debug message."
  (apply log 'DEBUG fmt args))

;;; log.scm ends here.

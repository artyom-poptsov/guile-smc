;;; guile.scm -- Guile-SMC state machine Guile compiler common procedures.

;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; The procedures in this module are for the Guile FSM compilers.


;;; Code:

(define-module (smc compiler guile-common)
  #:use-module (oop goops)
  #:use-module (ice-9 regex)
  #:use-module (smc version)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc core common)
  #:export (form-feed
            write-header
            write-parent-fsm-info))



(define (form-feed port)
  "Write a form feed symbol with the following newline to a PORT."
  (display #\ff port)
  (newline port))

(define (write-header port)
  "Write a header commentary to a @var{port}."
  (format port ";;; Generated by Guile-SMC ~a~%" (smc-version/string))
  (format port ";;; <https://github.com/artyom-poptsov/guile-smc>~%~%"))

(define-method-with-docs (write-parent-fsm-info (fsm <fsm>) (port <port>))
  "Print the information about the parent FSM for a @var{fsm} to a @var{port}."
  (form-feed port)
  (display ";;; This finite-state machine is produced by:\n" port)
  (for-each (lambda (line) (format port ";;;   ~a~%" line))
            (string-split (regexp-substitute/global #f
                                                    "\\\\n"
                                                    (fsm-description (fsm-parent fsm))
                                                    'pre "\n" 'post)
                          #\newline))
  (display ";;;\n" port)

  (fsm-pretty-print-statistics (fsm-parent fsm) port)
  (display ";;;\n" port)
  (puml-context-print-resolver-status (fsm-parent-context fsm)
                                      port)
  (newline port))

;;; guile-common.scm ends here.

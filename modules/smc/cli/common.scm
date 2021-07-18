;;; common.scm -- Common CLI procedures for Guile-SMC.

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

;; This module contains common CLI procedures for Guile-SMC.


;;; Code:

(define-module (smc cli common)
  #:use-module (ice-9 pretty-print)
  #:use-module (smc fsm)
  #:export (puml-modules
            add-to-load-path*
            eval-string/quote
            pretty-print-transition-table))


(define (pretty-print-transition-table fsm)
  "Pretty print the transition table for a specified @var{fsm}."
  (pretty-print (hash-table->transition-list (fsm-transition-table fsm))
                #:display? #t))


;; Core modules required to run the 'puml->fsm' converter.
(define %core-modules
  (list (resolve-module '(smc guards char))
        (resolve-module '(smc puml))
        (resolve-module '(smc fsm))))

(define (add-to-load-path* dirs)
  "Add all @var{dirs} to the load path list."
  (for-each (lambda (dir) (add-to-load-path dir)) dirs))

(define (eval-string/quote string)
  "Quote and evaluate a @var{string}."
  (eval-string (string-append "(quote " string ")")))



(define puml-modules
  (case-lambda
    (()
     %core-modules)
    ((modules-list)
     (if modules-list
         (append %core-modules
                 (map resolve-module
                      (eval-string/quote modules-list)))
         %core-modules))))

;;; common.scm ends here.

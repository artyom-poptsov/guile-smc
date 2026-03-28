 ;;; cli.scm -- Guile-SMC CLI tests.

;; Copyright (C) 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Test suite for Guile-SMC command-line interface.


;;; Code:

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc core common)
             (tests test-context))


(define %test-suite-name "core-common")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


(test-assert "safe-module-ref: #t"
  (procedure? (safe-module-ref (resolve-module '(smc core common))
                               'safe-module-ref)))

(test-equal "safe-module-ref: #f"
  #f
  (safe-module-ref (resolve-module '(smc core common))
                   'some-non-existing-procedure))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; core-common.scm ends here.


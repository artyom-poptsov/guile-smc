;;; cli.scm -- Guile-SMC CLI tests.

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

;; Test suite for Guile-SMC command-line interface.


;;; Code:

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc cli command-compile)
             (tests test-context))


(define %test-suite-name "cli")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)

;; compile

(test-assert "compile"
  (with-input-from-string
      (string-append
       "@startuml\n"
       "[*] -> state_1\n"
       "state_1: A state description.\n"
       "state_1 --> state_1: guard:#t\n"
       "@enduml\n")
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-compile '("smc")))))))

(test-assert "compile-validate"
  (with-input-from-string
      (string-append
       "@startuml\n"
       "[*] -> state_1\n"
       "state_1: A state description.\n"
       "state_1 --> state_1: guard:#t\n"
       "state_1 -> [*]: guard:eof-object?\n"
       "@enduml\n")
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-compile '("smc" "--validate")))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; cli.scm ends here.


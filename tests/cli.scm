;;; cli.scm -- Guile-SMC CLI tests.

;; Copyright (C) 2022-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (smc cli common)
             (smc cli command-compile)
             (smc cli command-context)
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
          (command-compile '("smc" "--log-driver" "null")))))))

(test-assert "compile-validate"
  (with-input-from-string
      (string-append
       "@startuml\n"
       "[*] -> state_1\n"
       "state_1: A state description.\n"
       "state_1 --> state_1: guard:#t\n"
       "state_1 -> [*]: char:eof-object?\n"
       "@enduml\n")
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-compile '("smc" "--log-driver" "null" "--validate")))))))

(test-error "compile-validate: error: no start tag"
  'puml-error
  (with-input-from-string
      (string-append
       "[*] -> state_1\n"
       "state_1: A state description.\n"
       "state_1 --> state_1: guard:#t\n"
       "state_1 -> [*]: char:eof-object?\n"
       "@enduml\n")
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-compile '("smc" "--log-driver" "null" "--validate")))))))

(test-error "compile-validate: error: no end tag"
  'puml-error
  (with-input-from-string
      (string-append
       "@startuml\n")
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-compile '("smc" "--log-driver" "null" "--validate")))))))

(test-error "compile-validate: error: unresolved procedure"
  'puml-error
  (with-input-from-string
      (string-append
       "@startuml\n"
       "[*] -> state_1\n"
       "state_1 --> state_1: guard:#t\n"
       "state_1 -> [*]: unknown-procedure\n"
       "@startuml\n")
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-compile '("smc" "--log-driver" "null" "--validate")))))))

(test-equal "cli-write-commentary: Single-line comment"
  ";; hello\n"
  (with-output-to-string
    (lambda ()
      (cli-write-commentary (current-output-port)
                            "hello"))))

(test-equal "cli-write-commentary: Multi-line comment"
  (string-append
   ";; hello\n"
   ";; world\n")
  (with-output-to-string
    (lambda ()
      (cli-write-commentary (current-output-port)
                            "hello"
                            "world"))))



(test-assert "command-context: oop"
  (let ((output (with-output-to-string
                  (lambda ()
                    (command-context
                     `("smc"
                       "--core-modules-path" ,(string-append (getenv "abs_top_srcdir")
                                                             "/modules/smc/")
                       "--log-driver" "null"
                       "-T" "oop"))))))
    (string-contains output "(smc context oop generic)")))

(test-assert "command-context: functional"
  (let ((output (with-output-to-string
                  (lambda ()
                    (command-context
                     `("smc"
                       "--core-modules-path" ,(string-append (getenv "abs_top_srcdir")
                                                             "/modules/smc/")
                       "--log-driver" "null"
                       "-T" "functional"))))))
    (string-contains output "(smc context functional char)")))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; cli.scm ends here.


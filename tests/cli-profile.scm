;;; cli-profiler.scm -- Guile-SMC CLI Profiler tests.

;; Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Test suite for Guile-SMC profiler.


;;; Code:

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc cli common)
             (smc trace-context)
             (smc cli command-profile)
             (tests test-context))


(define %test-suite-name "cli-profile")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)



(test-equal "print-human-readable"
  (string-join
   (list
    "Total transitions: 2"
    "Total time:        136 us"
    "Stats:"
    "  STATE-1: 136 us (100.0000 %)"
    "")
   "\n")
  (with-output-to-string
    (lambda ()
      (print-human-readable
       (list (make <log-entry:transition>
               #:level 'DEBUG
               #:timestamp '(#(54 28 7 1 4 123 1 120 -1 0 #f) . 19)
               #:timestamp-string "2023-05-01 07:28:54"
               #:timestamp-usec    -1682926133103737
               #:timestamp-relative 0
               #:transition-from "*"
               #:transition-to   "STATE-1")
             ;; level: DEBUG; timestamp: (#(54 28 7 1 4 123 1 120 -1 0 #f) . 19); timestamp-string: "2023-05-01 07:28:54"; timestamp-usec: -1682926133103601; timestamp-relative: 136
             (make <log-entry:transition>
               #:level 'DEBUG
               #:timestamp '(#(54 28 7 1 4 123 1 120 -1 0 #f) . 19)
               #:timestamp-string "2023-05-01 07:28:54"
               #:timestamp-usec    -1682926133103601
               #:timestamp-relative 136
               #:transition-from    "STATE-1"
               #:transition-to      "STATE-2"))
       136))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; cli-profiler.scm ends here.

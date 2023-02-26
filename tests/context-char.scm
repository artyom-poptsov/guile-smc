(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 binary-ports)
             (oop goops)
             (tests common)
             (smc context char))


(define %test-suite-name "context-char")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


;;; Guards.

(test-assert "guard:newline?"
  (guard:newline? 'context #\newline))

(test-assert "guard:space?"
  (guard:space? 'context #\space))

(test-assert "guard:letter?"
  (guard:letter? 'context #\a))

(test-assert "guard:single-quote?"
  (guard:single-quote? 'context #\'))

(test-assert "guard:colon?"
  (guard:colon? 'context #\:))

(test-assert "guard:left-square-bracket?"
  (guard:left-square-bracket? 'context #\[))

(test-assert "guard:right-square-bracket?"
  (guard:right-square-bracket? 'context #\]))

(test-assert "guard:at-symbol?"
  (guard:at-symbol? 'context #\@))

(test-assert "guard-digit?"
  (guard:digit? 'context #\1))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


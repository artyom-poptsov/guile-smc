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

(test-assert "char:newline?"
  (char:newline? 'context #\newline))

(test-assert "char:space?"
  (char:space? 'context #\space))

(test-assert "char:letter?"
  (char:letter? 'context #\a))

(test-assert "char:single-quote?"
  (char:single-quote? 'context #\'))

(test-assert "char:colon?"
  (char:colon? 'context #\:))

(test-assert "char:left-square-bracket?"
  (char:left-square-bracket? 'context #\[))

(test-assert "char:right-square-bracket?"
  (char:right-square-bracket? 'context #\]))

(test-assert "char:at-symbol?"
  (char:at-symbol? 'context #\@))

(test-assert "guard-digit?"
  (char:digit? 'context #\1))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


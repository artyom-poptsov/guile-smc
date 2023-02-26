(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 binary-ports)
             (oop goops)
             (tests common)
             (smc context u8))


(define %test-suite-name "context-u8")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


;;; Guards.

(test-assert "u8:nul?"
  (u8:nul? 'context 0))

(test-assert "u8:space?"
  (u8:space? 'context (char->integer #\space)))

(test-assert "u8:letter?"
  (u8:letter? 'context (char->integer #\a)))

(test-assert "u8:single-quote?"
  (u8:single-quote? 'context (char->integer #\')))

(test-assert "u8:colon?"
  (u8:colon? 'context (char->integer #\:)))

(test-assert "u8:left-square-bracket?"
  (u8:left-square-bracket? 'context (char->integer #\[)))

(test-assert "u8:right-square-bracket?"
  (u8:right-square-bracket? 'context (char->integer #\])))

(test-assert "u8:at-symbol?"
  (u8:at-symbol? 'context (char->integer #\@)))

(test-assert "u8:digit?"
  (u8:digit? 'context (char->integer #\1)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


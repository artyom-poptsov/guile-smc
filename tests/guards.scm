(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (smc guards char))


(test-begin "guards")

(test-assert "guard:#t"
  (guard:#t 'event 'context))

(test-assert "guard:newline?"
  (guard:newline? #\newline '()))

(test-assert "guard:space?"
  (guard:space? #\space '()))

(test-assert "guard:letter?"
  (guard:letter? #\a '()))

(test-assert "guard:single-quote?"
  (guard:single-quote? #\' '()))

(test-assert "guard:colon?"
  (guard:colon? #\: '()))

(test-assert "guard:left-square-bracket?"
  (guard:left-square-bracket? #\[ '()))

(test-assert "guard:right-square-bracket?"
  (guard:right-square-bracket? #\] '()))

(test-assert "guard:at-symbol?"
  (guard:at-symbol? #\@ '()))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "guards")

(exit exit-status)


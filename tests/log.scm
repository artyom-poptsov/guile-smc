(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (smc core log))


(define %test-name "log")
(test-begin %test-name)



(define-syntax-rule (test-match name regexp body)
  (test-assert name
    (string-match regexp body)))



(test-match "%precise-log-formatter"
  "2022-01-07 [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{6} \\(DEBUG\\): test\n"
  (%precise-log-formatter 'DEBUG '(1641586675 . 685611) "test"))

(test-equal "<port-log/us>: initialize"
  %precise-log-formatter
  (let ((h (make <precise-port-log>)))
    (slot-ref h 'formatter)))

(test-assert "precise-logger?"
  (precise-logger? (make <precise-logger>)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

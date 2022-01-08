(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (smc core log))


(define %test-name "log")
(test-begin %test-name)



(test-equal "%precise-log-formatter"
  "2022-01-07 23:17:55.685611 (DEBUG): test\n"
  (%precise-log-formatter 'DEBUG '(1641586675 . 685611) "test"))

;; (test-equal "<port-log/us>: initialize"
;;   %precise-log-formatter
;;   (let ((h (make <port-log/us>)))
;;     (slot-ref h 'formatter)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (smc fsm)
             (smc compiler guile)
             (smc context char)
             (smc context oop char)
             (tests common)
             (tests test-context))


(define %test-suite-name "compiler-guile")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)



(test-equal "write-define-class"
  "(define-class <test> (<fsm>))\n"
  (with-output-to-string
    (lambda ()
      (write-define-class '<test> (current-output-port)))))

(test-equal "write-module"
  (string-join
   (list
    "(define-module"
    "  (test module)"
    "  #:use-module"
    "  (oop goops)"
    "  #:use-module"
    "  (smc fsm)"
    "  #:re-export"
    "  (fsm-run!)"
    "  #:export"
    "  (test))"
    ""
    "")
   "\n")
  (with-output-to-string
    (lambda ()
      (write-module '(test module)
                    #:extra-modules '()
                    #:class-name 'test
                    #:port (current-output-port)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (srfi srfi-1)
             (oop goops)
             (smc fsm)
             (smc compiler guile-standalone)
             (tests common)
             (tests test-context))


(define %test-suite-name "compiler-guile-standalone")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)



(test-equal "fsm-get-class-code"
  '(define-class <test> ()
     (debug-mode?
      #:init-value   #f
      #:init-keyword #:debug-mode?
      #:getter       fsm-debug-mode?
      #:setter       fsm-debug-mode-set!))
  (fsm-get-class-code 'test))

(test-equal "prune-unused-definitions"
  '((define (f1 x) x))
  (prune-unused-definitions '((define (f1 x) x)
                              (define (f2 x) (f1 x))
                              (define (f3 x) x))
                            '()))

(test-equal "prune-unused-definitions: with hardwired definitions"
  '((define (f1 x) x)
    (define (f3 x) x))
  (prune-unused-definitions '((define (f1 x) x)
                              (define (f2 x) (f1 x))
                              (define (f3 x) x))
                            '(f3)))

(test-assert "fsm-get-context-code: type: #f"
  (find (lambda (exp)
          (and (equal? (car exp) 'define)
               (equal? (cadr exp) '(action:no-op ctx event))))
        (fsm-get-context-code (string-append (getenv "abs_top_srcdir") "/modules/smc/"))))

(test-assert "fsm-get-context-code: type: oop"
  (find (lambda (exp)
          (and (equal? (car exp) 'define-class)
               (equal? (cadr exp) '<char-context>)))
        (fsm-get-context-code (string-append (getenv "abs_top_srcdir") "/modules/smc/")
                              #:type 'oop)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


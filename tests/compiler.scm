(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (smc fsm)
             (smc compiler)
             (smc context char)
             (smc context oop char)
             (tests common)
             (tests test-context))


(define %test-suite-name "compiler")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)



(test-assert "fsm-compile"
  (let ((fsm (make <fsm>
               #:event-source char-context-event-source
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (,guard:#t ,action:no-op state-2)
                                      (,guard:#t ,action:no-op state-1)))
                                    ((name . state-2)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)
                                      (,guard:#t ,action:no-op state-3)))
                                    ((name . state-3)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)))))))
    (with-output-to-string
      (lambda ()
        (fsm-compile fsm)))))

(test-assert "fsm-compile: target: standalone"
  (let ((fsm (make <fsm>
               #:event-source char-context-event-source
               #:transition-table `(((name . state-1)
                                     (transitions
                                      (,guard:#t ,action:no-op state-2)
                                      (,guard:#t ,action:no-op state-1)))
                                    ((name . state-2)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)
                                      (,guard:#t ,action:no-op state-3)))
                                    ((name . state-3)
                                     (transitions
                                      (,guard:#t ,action:no-op state-1)))))))
    (with-output-to-string
      (lambda ()
        (fsm-compile fsm
                     #:target 'guile-standalone
                     #:modules-path (string-append (getenv "abs_top_srcdir")
                                                   "/modules/smc/"))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


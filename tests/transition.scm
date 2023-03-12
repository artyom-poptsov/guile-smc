(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (tests common)
             (smc context char)
             (smc core transition))


(define %test-name "transition")

(configure-test-logging! %test-name)
(test-begin %test-name)



(test-equal "transition:guard"
  guard:#t
  (transition:guard `(,guard:#t ,action:no-op 'next-state)))

(test-equal "transition:action"
  action:no-op
  (transition:action `(,guard:#t ,action:no-op 'next-state)))

(test-equal "transition:next-state"
  'next-state
  (transition:next-state `(,guard:#t ,action:no-op next-state)))



(test-equal "transition-table-append"
  `((,guard:#t ,action:no-op next-state))
  (transition-table-append '() guard:#t action:no-op 'next-state))

(test-equal "transition-table-count: all"
  2
  (transition-table-count (lambda (transition) #t)
                          (transition-table-append
                           (transition-table-append '()
                                                    guard:#t
                                                    action:no-op
                                                    'state-1)
                           guard:#t action:no-op 'state-2)))

(test-equal "transition-table-count: only state-1"
  1
  (transition-table-count (lambda (transition)
                            (equal? (transition:next-state transition)
                                    'state-1))
                          (transition-table-append
                           (transition-table-append '()
                                                    guard:#t
                                                    action:no-op
                                                    'state-1)
                           guard:#t action:no-op 'state-2)))



(define (guard:null? ctx event)
  (null? event))

(define (action:append-error ctx event)
  (cons 'error ctx))

(test-equal "transition-table-run"
  '(error)
  (let ((table `((,(const #f)  ,action:no-op state-1)
                 (,guard:null? ,action:append-error state-2))))
    (receive (next-state new-context)
        (transition-table-run table '() '())
      new-context)))



(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

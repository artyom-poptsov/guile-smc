(define-module (smc cli command-profile)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 r5rs)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc core set)
  #:use-module (smc compiler)
  #:use-module (smc fsm)
  #:use-module (smc puml)
  #:use-module (smc trace-context)
  #:use-module (smc trace-fsm)
  #:use-module (smc cli common)
  #:export (command-profile))



(define (print-help)
  (display "\
Usage: smc profile [options] <log-file>

Run a state machine.

Options:
  --help, -h        Print this message and exit.
  --debug           Enable the debug mode.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))
    (debug                                      (value #f))))

(define-method (trace-profile-time (trace <list>))
  (let loop ((prev-tr    #f)
             (trs        trace)
             (result     (make-hash-table)))
    (if (null? trs)
        result
        (let ((tr (car trs)))
          (if (log-entry:message? tr)
              (loop prev-tr (cdr trs) result)
              (if prev-tr
                  (let* ((prev-state (log-entry-transition-to prev-tr))
                         (time       (hash-ref result prev-state)))
                    (begin
                      (hash-set! result
                                 prev-state
                                 (+ (if time
                                        time
                                        0)
                                    (- (log-entry-timestamp-usec tr)
                                       (log-entry-timestamp-usec prev-tr))))
                      (loop tr (cdr trs) result)))
                  (begin
                    (loop tr (cdr trs) result))))))))



(define (command-profile args)
  (let* ((options          (getopt-long args %option-spec))
         (debug-mode?      (option-ref options 'debug     #f))
         (args             (option-ref options '()        #f)))

    (when (or (option-ref options 'help #f) (null? args))
      (print-help)
      (exit 0))

    (log-use-stderr! debug-mode?)

    (let* ((port    (open-input-file (car args)))
           (context (fsm-run! (make <trace-fsm>
                                #:debug-mode? debug-mode?)
                              (make <trace-context>
                                #:port port)))
           (trace   (reverse (trace-context-result context)))
           (total-time (- (log-entry-timestamp-usec (car (trace-context-result context)))
                          (log-entry-timestamp-usec (car trace)))))
      (format #t "Total transitions: ~a~%" (length trace))
      (format #t "Total time:        ~a~%" total-time)
      (display "Stats:\n")
      (for-each (lambda (kv)
                  (unless (string=? (car kv) "*")
                    (format #t
                            "  ~a: ~a us (~2,4f %)~%"
                            (car kv)
                            (cdr kv)
                            (* (/ (cdr kv) total-time) 100))))
                (sort (hash-map->list cons (trace-profile-time trace))
                      (lambda (kv1 kv2)
                        (> (cdr kv1) (cdr kv2))))))))

;;; command-profile.scm ends here.

#!/usr/bin/guile \
-L modules -e main -s
!#

;; Run it:
;;   ./examples/pumlpuml.scm ./modules/smc/puml.puml input.puml

(use-modules (ice-9 pretty-print)
             (ice-9 receive)
             (ice-9 textual-ports)
             (oop goops)
             (smc puml)
             (smc core log)
             (smc fsm))


(define (print-help-and-exit prog)
  "Print the help message and exit from the program."
  (format #t "Usage: ~a <source-puml-file> <file-to-parse>~%" prog)
  (exit 1))


(define (main args)

  (when (< (length args) 3)
    (print-help-and-exit (car args)))

  (let ((fsm (puml->fsm (open-input-file (list-ref args 1))
                        #:module (list (resolve-module '(smc context char-context))
                                       (resolve-module '(smc puml))
                                       (resolve-module '(smc fsm)))
                        #:debug-mode? #t))
        (p (open-input-file (list-ref args 2))))
    (format #t "output fsm:             ~a~%" fsm)
    (when (fsm-description fsm)
      (format #t "  description:          ~a~%" (fsm-description fsm)))
    (format #t "  state count: ~s~%" (fsm-state-count fsm))
    (format #t "  validation result: ~%")
    (pretty-print (fsm-validate fsm))
    (format #t "transition table (count: ~a):~%" (fsm-transition-count fsm))
    (pretty-print
     (hash-table->transition-list (fsm-transition-table fsm))
     #:display? #t)
    ;; (log-use-stderr! #t)
    ;; (fsm-debug-mode-set! fsm #t)
    (let* ((context (make <puml-context> #:module (list (resolve-module '(smc context char-context))
                                                        (resolve-module '(smc puml))
                                                        (resolve-module '(smc fsm)))))
           (new-context (fsm-run! fsm
                                  (lambda (context) (get-char p))
                                  context)))
      (let ((new-fsm (puml-context-fsm new-context)))
        (format #t "~%-----statistics: ------~%")
        (pretty-print (fsm-statistics fsm))
        (format #t "~%----- new FSM output: -----~%")
        (pretty-print
         (hash-table->transition-list (fsm-transition-table new-fsm))
         #:display? #t)))))

#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (oop goops)
             (ice-9 receive)
             (ice-9 textual-ports)
             (smc fsm)
             (smc core log)
             (smc core state))

(define (fsm-eof ch ctx)
  (and (eof-object? ch) (list->string (reverse ctx))))

(define (fsm-space ch ctx)
  (and (char=? ch #\space)
       ctx))

(define (fsm-newline ch ctx)
  (and (char=? ch #\newline)
       (cons #\newline ctx)))

(define (fsm-read-char ch ctx)
  (and (not (char=? ch #\space))
       (cons ch ctx)))

(define (fsm-skip ch ctx)
  ctx)

(define (main args)
  (let ((fsm (make <fsm>)))
    (log-use-stderr! #t)
    (fsm-state-add! fsm (make <state> #:name 'before))
    (fsm-state-add! fsm (make <state> #:name 'inside))
    (fsm-state-add! fsm (make <state> #:name 'after))
    (fsm-transition-add! fsm 'before
                         `((,fsm-eof       . #f)
                           (,fsm-read-char . inside)
                           (,fsm-space     . before)))

    (fsm-transition-add! fsm 'inside
                         `((,fsm-eof       . #f)
                           (,fsm-space     . after)
                           (,fsm-read-char . inside)))

    (fsm-transition-add! fsm 'after
                         `((,fsm-eof       . #f)
                           (,fsm-newline   . before)
                           (,fsm-skip      . after)))

    (fsm-current-state-set! fsm (fsm-state fsm 'before))

    (let loop ((context '()))
      (receive (new-state new-context)
          (fsm-run! fsm (get-char (current-input-port)) context)
        (if new-state
            (loop new-context)
            (format #t "result: ~a~%" new-context))))))


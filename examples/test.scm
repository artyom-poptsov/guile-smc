#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (oop goops)
             (ice-9 receive)
             (ice-9 textual-ports)
             (smc fsm)
             (smc core log)
             (smc core state))


;; Guards

(define (fsm-eof-object? ch ctx)
  (eof-object? ch))

(define (fsm-space? ch ctx)
  (char=? ch #\space))

(define (fsm-non-space? ch ctx)
  (not (char=? ch #\space)))

(define (fsm-newline? ch ctx)
  (char=? ch #\newline))


;; Actions

(define (fsm-end ch ctx)
  (list->string (reverse ctx)))

(define (fsm-noop ch ctx)
  ctx)

(define (fsm-cons ch ctx)
  (cons ch ctx))



(define (main args)
  (let ((fsm (make <fsm>)))
    (log-use-stderr! #t)
    (fsm-state-add! fsm (make <state> #:name 'before))
    (fsm-state-add! fsm (make <state> #:name 'inside))
    (fsm-state-add! fsm (make <state> #:name 'after))
    (fsm-transition-add! fsm 'before
                         `((,fsm-eof-object? ,fsm-end  #f)
                           (,fsm-non-space?  ,fsm-cons inside)
                           (,fsm-space?      ,fsm-noop before)))

    (fsm-transition-add! fsm 'inside
                         `((,fsm-eof-object? ,fsm-end  #f)
                           (,fsm-space?      ,fsm-noop after)
                           (,fsm-non-space?  ,fsm-cons inside)))

    (fsm-transition-add! fsm 'after
                         `((,fsm-eof-object? ,fsm-end  #f)
                           (,fsm-newline?    ,fsm-cons before)
                           (,(const #t)      ,fsm-noop after)))

    (fsm-current-state-set! fsm (fsm-state fsm 'before))

    (let loop ((context '()))
      (receive (new-state new-context)
          (fsm-run! fsm (get-char (current-input-port)) context)
        (if new-state
            (loop new-context)
            (format #t "result: ~a~%" new-context))))))


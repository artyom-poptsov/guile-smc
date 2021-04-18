#!/usr/bin/guile \
-L modules -e main -s
!#

(use-modules (oop goops)
             (ice-9 receive)
             (ice-9 textual-ports)
             (smc fsm)
             (smc core log)
             (smc guards char)
             (smc core state))


;; Guards

(define (guard:non-space? ch ctx)
  (not (char=? ch #\space)))


;; Actions

(define (action:end ch ctx)
  (list->string (reverse ctx)))

(define (action:cons ch ctx)
  (cons ch ctx))



(define (main args)
  (log-use-stderr! #t)
  (let ((fsm (make <fsm>
               #:debug-mode? #t
               #:transition-table
               `((before
                  (,guard:eof-object? ,action:end   #f)
                  (,guard:non-space?  ,action:cons  inside)
                  (,guard:space?      ,action:no-op before))
                 (inside
                  (,guard:eof-object? ,action:end   #f)
                  (,guard:space?      ,action:no-op after)
                  (,guard:non-space?  ,action:cons  inside))
                 (after
                  (,guard:eof-object? ,action:end   #f)
                  (,guard:newline?    ,action:cons  before)
                  (,(const #t)        ,action:no-op after))))))

    (let loop ((context '()))
      (receive (new-state new-context)
          (fsm-run! fsm (get-char (current-input-port)) context)
        (if new-state
            (loop new-context)
            (format #t "result: ~a~%" new-context))))))


;; This module contains some common state machine transition guards.

(define-module (smc guards char)
  #:export (guard:#t
            guard:newline?
            guard:space?
            guard:letter?
            guard:eof-object?))

(define (guard:#t event ctx)
  "This guard is always returns #t."
  #t)

(define (guard:newline? ch ctx)
  (char=? ch #\newline))

(define (guard:space? ch ctx)
  (char=? ch #\space))

(define (guard:letter? ch ctx)
  (char-set-contains? char-set:letter ch))

(define (guard:eof-object? ch ctx)
  (eof-object? ch))

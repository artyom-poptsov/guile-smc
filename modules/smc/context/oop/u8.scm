;;; u8.scm -- Guile-SMC finite state machine u8 context.

;; Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This file contains an implementation of a binary context based on a
;; generic context. The binary context can be used to handle a stream of
;; bytes.


;;; Code:

(define-module (smc context oop u8)
  #:use-module (ice-9 binary-ports)
  #:use-module (oop goops)
  #:use-module (smc context oop generic)
  #:use-module (smc context oop port)
  #:use-module (smc core log)
  #:re-export (;; From (smc context context)
               ;; and (smc context port)
               <context>
               context?
               context-debug-mode?
               context-debug-mode-set!
               context-port
               context-counter
               context-stanza
               context-stanza/reversed
               context-stanza-set!
               context-stanza-add!
               context-stanza-clear!
               context-buffer
               context-buffer/reversed
               context-buffer-set!
               context-buffer-add!
               context-buffer-clear!
               push-event-to-buffer
               push-event-to-stanza
               push-event-to-result
               push-buffer-to-stanza
               push-stanza-to-result
               buffer-empty?
               update-counter
               pop-buffer
               pop-stanza
               pop-result
               clear-buffer
               clear-stanza
               clear-result)
  #:export (<u8-context>
            u8-context?
            u8-context-update-counters!

            u8-context-event-source

            ;; Actions.
            u8-context-syntax-error

            ;; All guards that are exported with 'define-public' below.

            ;; Logging procedures
            u8-context-log-error
            u8-context-log-warning
            u8-context-log-info
            u8-context-log-debug))


(define-class <u8-context> (<port-context>))



(define (u8-context? x)
  "Check if X is an <u8-context> instance."
  (is-a? x <u8-context>))

;; Update counters in a character context CTX based on an incoming character
;; CH.  These counters are thrown when a syntax error occurred.
(define-method (u8-context-update-counters! (ctx <u8-context>) byte)
  (unless (eof-object? byte)
    (context-counter-update! ctx)))


;;; Event source.

;; Get the next character from a CONTEXT port.
(define-method (u8-context-event-source (context <u8-context>))
  (let ((byte (get-u8 (context-port context))))
    (u8-context-update-counters! context byte)
    byte))




(define (u8-context-syntax-error ctx byte)
  (error "Syntax error"
         (context-port ctx)
         (context-counter ctx)
         byte
         ctx))



(define (%current-position-prefix ctx)
  (format #f "~a:~a: "
          (context-port ctx)
          (context-counter ctx)))

(define (u8-context-log-error ctx fmt . rest)
  (apply log-error
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (u8-context-log-warning ctx fmt . rest)
  (apply log-warning
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (u8-context-log-info ctx fmt . rest)
  (apply log-info
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (u8-context-log-debug ctx fmt . rest)
  (apply log-debug
         (string-append (%current-position-prefix ctx) fmt)
         rest))

;;; u8.scm ends here.

;;; u8.scm -- Guile-SMC functional u8 binary context for FSMs.

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

;; This file contains functional u8 binary port context for finite-state
;; machines.  This context helps to read and parse a stream of bytes from a
;; specified port.


;;; Code:

(define-module (smc context functional u8)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 binary-ports)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:use-module ((smc context functional generic)
                #:renamer (symbol-prefix-proc 'generic:))
  #:export (<u8-context>
            u8-context?
            make-u8-context

            context-debug-mode?
            context-port

            context-counter
            context-counter-set
            context-counter-update
            context-row-number
            context-row-number-set
            context-row-number-update
            context-col-number
            context-col-number-set
            context-col-number-update
            context-buffer
            context-buffer/reversed
            context-buffer-set
            context-buffer-append
            context-stanza
            context-stanza/reversed
            context-stanza-set
            context-result
            context-result/reversed
            context-result-set

            ;; Event source.
            next-char

            ;; Actions.
            clear-buffer
            clear-stanza
            clear-result
            reverse-buffer
            reverse-stanza
            reverse-result
            push-event-to-buffer
            push-event-to-stanza
            push-event-to-result
            push-buffer-to-stanza
            push-stanza-to-result
            pop-buffer
            pop-stanza
            pop-result
            update-counter
            update-row-number
            update-col-number
            reset-col-number
            throw-syntax-error

            ;; Logging procedures
            context-log-error
            context-log-warning
            context-log-info
            context-log-debug

            ;; Low-level procedures.
            %make-u8-context))



(define-immutable-record-type <u8-context>
  (%make-u8-context parent-context
                    port)
  u8-context?

  ;; <context>
  (parent-context context-parent context-parent-set)

  ;; Context port that is used to read the data.
  ;;
  ;; <port>
  (port context-port context-port-set))

(define* (make-u8-context #:key
                            (port (current-input-port))
                            (debug-mode? #f)
                            (counter 0)
                            (buffer '())
                            (stanza '())
                            (result '()))
  (%make-u8-context (generic:make-context #:debug-mode? debug-mode?
                                          #:counter     counter
                                          #:buffer      buffer
                                          #:stanza      stanza
                                          #:result      result)
                    port))



(set-record-type-printer!
 <u8-context>
 (lambda (record port)
   (format port
           "#<u8-context ~a>"
           (object-address/hex-string record))))



(define (next-u8 context)
  "Event source."
  (get-u8 (context-port context)))





;; Parent accessors/setters.
(generic:%make-parent-accessor context-debug-mode?)
(generic:%make-parent-setter   context-debug-mode-set)
(generic:%make-parent-accessor context-counter)
(generic:%make-parent-setter   context-counter-set)
(generic:%make-parent-accessor context-buffer)
(generic:%make-parent-setter   context-buffer-set)
(generic:%make-parent-accessor context-stanza)
(generic:%make-parent-setter   context-stanza-set)
(generic:%make-parent-accessor context-result)
(generic:%make-parent-setter   context-result-set)

;; Parent actions.
(generic:%make-parent-action push-event-to-buffer)
(generic:%make-parent-action push-event-to-stanza)
(generic:%make-parent-action push-event-to-result)
(generic:%make-parent-action push-buffer-to-stanza)
(generic:%make-parent-action push-stanza-to-result)
(generic:%make-parent-action update-counter)
(generic:%make-parent-action pop-buffer)
(generic:%make-parent-action pop-stanza)
(generic:%make-parent-action pop-result)
(generic:%make-parent-action clear-buffer)
(generic:%make-parent-action clear-stanza)
(generic:%make-parent-action clear-result)



(define* (context-counter-update context #:key (delta 1))
  (context-counter-set context (+ (context-counter context) delta)))


;;; Logging.

(define (%current-position-prefix ctx)
  "Make a char context CTX prefix for a log message.  Return the prefix as a
string."
  (format #f
          "~a:~a: "
          (context-port ctx)
          (context-counter ctx)))

(define (context-log-error ctx fmt . rest)
  (apply log-error
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (context-log-warning ctx fmt . rest)
  (apply log-warning
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (context-log-info ctx fmt . rest)
  (apply log-info
         (string-append (%current-position-prefix ctx) fmt)
         rest))

(define (context-log-debug ctx fmt . rest)
  (apply log-debug
         (string-append (%current-position-prefix ctx) fmt)
         rest))


;;; Error reporting.

(define (throw-syntax-error context u8)
  (error "Syntax error"
         (context-port context)
         (context-counter context)
         u8
         context))

;;; char.scm ends here.

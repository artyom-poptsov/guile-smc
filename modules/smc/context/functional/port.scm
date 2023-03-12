;;; port.scm -- Guile-SMC functional port context for FSMs.

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

;; This file contains functional port context for finite-state machines.  This
;; context helps to read and parse data from a specified port.


;;; Code:

(define-module (smc context functional port)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (smc core common)
  #:use-module (smc core log)
  #:use-module ((smc context functional generic)
                #:renamer (symbol-prefix-proc 'generic:))
  #:export (<port-context>
            make-port-context
            port-context?

            context-port
            context-port-set
            context-debug-mode?
            context-debug-mode-set
            context-counter
            context-counter-set
            context-counter-update
            context-buffer
            context-buffer/reversed
            context-buffer-set
            context-stanza
            context-stanza/reversed
            context-stanza-set
            context-result
            context-result/reversed
            context-result-set

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
            update-counter
            update-result
            update-stanza
            reset-buffer
            reset-stanza
            throw-error

            ;; Event source.
            next-u8

            ;; Low-level procedures.
            %make-port-context))



(define-immutable-record-type <port-context>
  (%make-port-context parent-context port)
  port-context?
  ;; <context>
  (parent-context context-parent context-parent-set)

  ;; Context port that is used to read the data.
  ;;
  ;; <port>
  (port        context-port))

(define* (make-port-context #:key
                            (port (current-input-port))
                            (debug-mode? #f)
                            (counter 0)
                            (buffer '())
                            (stanza '())
                            (result '()))
  (%make-port-context (generic:make-context #:debug-mode? debug-mode?
                                            #:counter     counter
                                            #:buffer      buffer
                                            #:stanza      stanza
                                            #:result      result)
                      port))



(set-record-type-printer!
 <port-context>
 (lambda (record port)
   (format port
           "#<port-context ~a>"
           (object-address/hex-string record))))



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



(define (next-u8 context)
  "Event source."
  (get-u8 (context-port context)))



(define throw-error generic:throw-error)

;;; port.scm ends here.

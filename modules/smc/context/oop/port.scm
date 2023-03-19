;;; port.scm -- Guile-SMC finite state machine port context.

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

;; This file contains an implementation of a port context that can be used
;; with an FSM that reads data from a port.


;;; Code:

(define-module (smc context oop port)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc context oop generic)
  #:re-export (context-counter
               context-counter-set!
               context-counter-update!
               context-stanza
               context-stanza/reversed
               context-stanza-set!
               context-buffer
               context-buffer/reversed
               context-buffer-set!
               context-result
               context-result-set!
               context-result/reversed

               ;; Guards.
               buffer-empty?
               stanza-empty?
               result-empty?

               ;; Actions.
               pop-buffer
               pop-stanza
               pop-result
               clear-buffer
               clear-stanza
               clear-result
               update-counter
               push-event-to-buffer
               push-event-to-stanza
               push-event-to-result
               push-buffer-to-stanza
               push-stanza-to-result)
  #:export (<port-context>
            port-context?
            context-port))



(define-class <port-context> (<context>)
  ;; A port from which data is read.
  ;;
  ;; <port>
  (port
   #:init-thunk   (lambda () (current-input-port))
   #:init-keyword #:port
   #:getter       context-port))



(define (port-context? x)
  "Check if X is a <port-context> instance."
  (is-a? x <port-context>))

;;; port.scm ends here.

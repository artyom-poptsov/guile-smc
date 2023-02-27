;;; common.scm -- Guile-SMC finite state machine common guards.

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

;; This file contains common guard/action procedures that are independent of
;; context type.


;;; Code:

(define-module (smc context common)
  #:export (guard:#t
            action:no-op))


(define (action:no-op ctx event)
  "The action that does nothing special, just returns the context CTX as it is."
  ctx)

(define (guard:#t ctx event)
  "This guard is always returns #t."
  #t)

;;; common.scm ends here.

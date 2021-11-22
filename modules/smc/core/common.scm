;;; log.scm -- Guile-SMC common code.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains common Guile-SMC code.


;;; Code:

(define-module (smc core common)
  #:use-module (scheme documentation)
  #:export (define-method-with-docs
             object-address/hex-string
             safe-module-ref))

(define (safe-module-ref module proc-name)
  (catch #t
    (lambda ()
      (module-ref module proc-name))
    (const #f)))

(define (object-address/hex-string object)
  (number->string (object-address object) 16))

(define-macro-with-docs (define-method-with-docs name-and-args docs . body)
  "Define a method with documentation."
  `(begin
     (define-method ,name-and-args ,@body)
     (set-object-property! ,(car name-and-args) 'documentation ,docs)
     *unspecified*))

;;; common.scm ends here.

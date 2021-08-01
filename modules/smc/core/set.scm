;;; set.scm -- Naive implementation of the "set" data structure.

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

;; This file contains a naive implementation of the "set" data structure.


;;; Code:

(define-module (smc core set)
  #:use-module (smc core common)
  #:use-module (oop goops)
  #:export (<set>
            set?
            set-content
            set-add!
            set-remove!
            set-capacity
            set-empty?
            set-contains?))



;; This class describes the "set" data structure.
(define-class <set> ()
  ;; The set content.
  ;;
  ;; <list>
  (content
   #:init-value '()
   #:getter     set-content
   #:setter     set-content-set!))



(define (set? x)
  (is-a? x <set>))


(define-method (%display (set <set>) (port <port>))
  (format port "#<set capacity: ~a ~a>"
          (set-capacity set)
          (object-address/hex-string set)))

(define-method (display (set <set>) (port <port>))
  (%display set port))

(define-method (write (set <set>) (port <port>))
  (%display set port))



;; Add an ELEMENT to the SET.
(define-method (set-add! (set <set>) element)
  (let ((content (set-content set)))
    (unless (member element content)
      (set-content-set! set (cons element content)))))

;; Remove an ELEMENT from the SET.
(define-method (set-remove! (set <set>) element)
  (let ((content (set-content set)))
    (unless (member element content)
      (set-content-set! set (delete element content)))))

;; Get the SET capacity.
(define-method (set-capacity (set <set>))
  (length (set-content set)))

;; Check if a SET contains an ELEMENT.
(define-method (set-contains? (set <set>) element)
  (if (member element (set-content set))
      #t
      #f))

(define-method (set-empty? (set <set>))
  (zero? (set-capacity set)))

;;; set.scm ends here.

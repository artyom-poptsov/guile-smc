(define-module (smc context functional generic)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (smc core common)
  #:export (<context>
            context?
            context-debug-mode?
            context-debug-mode-set
            context-result
            context-result/reversed
            context-result-set
            context-result-append

            ;; Actions.
            append-result
            update-counter
            throw-error))



(define-immutable-record-type <context>
  (%make-context debug-mode? result)
  context?
  (debug-mode? context-debug-mode? context-debug-mode-set)
  (counter     context-counter     context-counter-set)
  (result      context-result      context-result-set))



(set-record-type-printer!
 <context>
 (lambda (record port)
   (format port
           "#<context ~a>"
           (object-address/hex-string record))))



(define (context-result-append context event)
  (context-result-set context (cons event (context-result context))))

(define (context-result/reversed context)
  (reverse (context-result context)))

(define* (context-counter-update context #:key (delta 1))
  (context-counter-set context (+ (context-counter context) delta)))


;;; Actions.

(define (update-counter context event)
  (context-counter-update context))

(define (append-result ctx event)
  ((context-result-append ctx event)))


;;; Error reporting.

(define (throw-error context event)
  (error "Context error" context event))

;;; context.scm ends here.

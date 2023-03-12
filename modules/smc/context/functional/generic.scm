(define-module (smc context functional generic)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (smc core common)
  #:export (<context>
            %make-context
            make-context
            context?
            context-debug-mode?
            context-debug-mode-set
            context-counter
            context-counter-update
            context-buffer
            context-buffer-set
            context-stanza
            context-stanza-set
            context-result
            context-result/reversed
            context-result-set
            context-result-append

            ;; Actions.
            clear-buffer
            clear-stanza
            clear-result
            push-event-to-buffer
            push-event-to-stanza
            push-event-to-result
            push-buffer-to-stanza
            push-stanza-to-result
            pop-buffer
            pop-stanza
            pop-result
            update-counter
            throw-error))



(define-immutable-record-type <context>
  (%make-context debug-mode? counter buffer stanza result)
  context?

  ;; Flag that specifies whether the debug mode for the context is enabled.
  ;;
  ;; <boolean>
  (debug-mode? context-debug-mode? context-debug-mode-set)

  ;; Context counter.  Can be used to count incoming events, for example.
  ;;
  ;; <number>
  (counter     context-counter     context-counter-set)

  ;; Context buffer to store intermediate values.
  ;;
  ;; <list>
  (buffer      context-buffer      context-buffer-set)

  ;; Context stanza to store the chunks of intermediate context data.
  ;;
  ;; <list>
  (stanza      context-stanza      context-stanza-set)

  ;; Context result to store the end result of the parser.
  ;;
  ;; <list>
  (result      context-result      context-result-set))


(define* (make-context #:key
                       (debug-mode? #f)
                       (counter 0)
                       (buffer '())
                       (stanza '())
                       (result '()))
  "<context> constructor."
  (%make-context debug-mode?
                 counter
                 buffer
                 stanza
                 result))



(set-record-type-printer!
 <context>
 (lambda (record port)
   (format port
           "#<context ~a>"
           (object-address/hex-string record))))



(define (context-result/reversed context)
  (reverse (context-result context)))

(define* (context-counter-update context #:optional (delta 1))
  (context-counter-set context (+ (context-counter context) delta)))


;;; Actions.

(define (clear-buffer context event)
  "Set the CONTEXT buffer to an empty list.  Return the updated context."
  (context-buffer-set context '()))

(define (clear-stanza context event)
  "Set the CONTEXT stanza to an empty list.  Return the updated context."
  (context-stanza-set context '()))

(define (clear-result context event)
  "Set the CONTEXT result to an empty list.  Return the updated context."
  (context-result-set context '()))

(define (update-counter context event)
  "Update the CONTEXT counter.  Return the updated context."
  (context-counter-update context))

(define (push-event-to-buffer context event)
  "Push an EVENT to the CONTEXT buffer.  Return the updated context."
  (context-buffer-set context (cons event (context-buffer context))))

(define (push-event-to-stanza context event)
  "Push an EVENT to the CONTEXT stanza.  Return the updated context."
  (context-stanza-set context (cons event (context-stanza context))))

(define (push-event-to-result context event)
  "Push an EVENT to the CONTEXT result.  Return the updated context."
  (context-result-set context (cons event (context-result context))))

(define (push-buffer-to-stanza context event)
  "Push the CONTEXT buffer content to the CONTEXT stanza and clear the CONTEXT buffer.
Return the updated context."
  (clear-buffer (context-stanza-set context
                                    (cons (context-buffer context)
                                          (context-stanza context)))
                event))

(define (push-stanza-to-result context event)
  "Push the CONTEXT stanza content to the CONTEXT result and clear the CONTEXT stanza.
Return the updated context."
  (clear-stanza context
                (context-result-set context
                                    (cons (context-stanza context)
                                          (context-result context)))))

(define (pop-buffer context event)
  "Remove the last element of CONTEXT buffer.  Return the updated context."
  (context-buffer-set context (cdr (context-buffer context))))

(define (pop-stanza context event)
  "Remove the last element of CONTEXT stanza.  Return the updated context."
  (context-stanza-set context (cdr (context-stanza context))))

(define (pop-result context event)
  "Remove the last element of CONTEXT result.  Return the updated context."
  (context-result-set context (cdr (context-result context))))


;;; Error reporting.

(define (throw-error context event)
  (error "Context error" context event))

;;; context.scm ends here.
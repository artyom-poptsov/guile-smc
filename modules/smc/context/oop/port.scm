(define-module (smc context oop port)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc context oop generic)
  #:export (<port-context>
            port-context?
            context-port
            context-counter
            context-counter-set!
            context-counter-update!
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
            context-result
            context-result-set!
            context-clear!

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
            push-stanza-to-result
            action:store
            action:clear-buffer
            action:update-stanza))



(define-class <port-context> (<context>)
  ;; A port from which data is read.
  ;;
  ;; <port>
  (port
   #:init-thunk   (lambda () (current-input-port))
   #:init-keyword #:port
   #:getter       context-port)

  ;; Total number of objects read.
  ;;
  ;; <number>
  (counter
   #:init-value 0
   #:getter     context-counter
   #:setter     context-counter-set!)

  ;; The buffer holds read symbols.
  ;;
  ;; <list>
  (buffer
   #:init-value '()
   #:init-keyword #:buffer
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  ;; The stanza holds a logical unit of parsing (e.g. a key/value pair)
  ;;
  ;;<list>
  (stanza
   #:init-value '()
   #:init-keyword #:stanza
   #:getter     context-stanza
   #:setter     context-stanza-set!)

  ;; The context result holds the result of the FSM work.
  ;;
  ;; <list> | <top>
  (result
   #:init-value '()
   #:init-keyword #:result
   #:getter     context-result
   #:setter     context-result-set!))



(define (port-context? x)
  (is-a? x <port-context>))



(define-method (context-counter-update! (ctx <port-context>) (delta <number>))
  (context-counter-set! ctx (+ (context-counter ctx) delta)))

(define-method (context-counter-update! (ctx <port-context>))
  (context-counter-update! ctx 1))

(define-method (context-buffer/reversed (context <port-context>))
  (reverse (context-buffer context)))

(define-method (context-stanza/reversed (context <port-context>))
  (reverse (context-stanza context)))



(define* (pop-buffer context #:optional event)
  (context-buffer-set! context (cdr (context-buffer context))))

(define* (pop-stanza context #:optional event)
  (context-stanza-set! context (cdr (context-stanza context))))

(define* (pop-result context #:optional event)
  (context-result-set! context (cdr (context-result context))))



(define* (clear-buffer context #:optional event)
  (context-buffer-set! context '())
  context)

(define* (clear-stanza context #:optional event)
  (context-stanza-set! context '())
  context)

(define* (clear-result context #:optional event)
  (context-result-set! context '())
  context)



(define* (reverse-buffer context #:optional event)
  (context-buffer-set! context (reverse (context-buffer context)))
  context)

(define* (reverse-stanza context #:optional event)
  (context-stanza-set! context (reverse (context-stanza context)))
  context)

(define* (reverse-result context #:optional event)
  (context-result-set! context (reverse (context-result context)))
  context)



(define (push-event-to-buffer context event)
  "Push a new EVENT in a CONTEXT buffer."
  (when (context-debug-mode? context)
    (log-debug "push-event-to-buffer: event: ~a; buffer: ~a"
               event
               (context-buffer context)))
  (context-buffer-set! context (cons event (context-buffer context)))
  context)

(define (push-event-to-stanza context event)
  "Push a new EVENT in a CONTEXT stanza."
  (when (context-debug-mode? context)
    (log-debug "push-event-to-stanza: event: ~a; stanza: ~a"
               event
               (context-stanza context)))
  (context-stanza-set! context (cons event (context-stanza context)))
  context)

(define (push-event-to-result context event)
  "Push a new EVENT in a CONTEXT result."
  (when (context-debug-mode? context)
    (log-debug "push-event-to-result: event: ~a; result: ~a"
               event
               (context-result context)))
  (context-result-set! context (cons event (context-result context)))
  context)

(define (push-buffer-to-stanza context event)
  "Push the CONTEXT buffer contents to a CONTEXT stanza."
  (when (context-debug-mode? context)
    (log-debug "push-buffer-to-stanza: event: ~a; buffer: ~a; stanza: ~a"
               event
               (context-buffer context)
               (context-stanza context)))
  (context-result-set! context (cons (context-stanza context)
                                     (context-result context)))
  (clear-buffer context event))

(define (push-stanza-to-result context event)
  "Push the CONTEXT stanza contents to a CONTEXT result."
  (when (context-debug-mode? context)
    (log-debug "push-buffer-to-result: event: ~a; buffer: ~a; stanza: ~a"
               event
               (context-buffer context)
               (context-stanza context)))
  (context-result-set! context (cons (context-stanza context)
                                     (context-result context)))
  (clear-stanza context event))


(define-method (context-buffer-clear! (ctx <port-context>))
  "Clear the context CTX buffer."
  (context-buffer-set! ctx '()))

(define-method (context-buffer-add! (ctx <port-context>) value)
  "Add a new VALUE to the context CTX buffer."
  (context-buffer-set! ctx
                       (cons value (context-buffer ctx))))

(define-method (context-stanza-clear! (ctx <port-context>))
  "Clear the context CTX stanza."
  (context-stanza-set! ctx '()))

(define-method (context-stanza-add! (ctx <port-context>) value)
  "Add a new VALUE to the context CTX stanza."
  (context-stanza-set! ctx
                       (cons value (context-stanza ctx))))

(define-method (context-clear! (ctx <port-context>))
  "Clear both the context CTX buffer and stanza."
  (context-buffer-clear! ctx)
  (context-stanza-clear! ctx))


;;; Actions.

(define (update-counter ctx event)
  (context-counter-update! ctx))

(define (push-event-to-buffer context event)
  "Push a new EVENT in a CONTEXT buffer."
  (when (context-debug-mode? context)
    (log-debug "push-to-buffer: event: ~a; buffer: ~a"
               event
               (context-buffer context)))
  (context-buffer-set! context (cons event (context-buffer context)))
  context)

(define (action:clear-buffer ctx event)
  "Clear the context CTX buffer."
  (context-buffer-clear! ctx)
  ctx)

(define (action:update-stanza ctx event)
  "Copy the context CTX buffer to the stanza, clear the buffer."
  (let ((buf (reverse (context-buffer ctx))))
    (unless (null? buf)
      (when (context-debug-mode? ctx)
        (let ((stanza (reverse (context-stanza ctx))))
          (log-debug "action:update-stanza: event: ~a; buffer: ~a; stanza: ~a"
                     event
                     buf
                     stanza)))
      (context-stanza-add! ctx buf)
      (context-buffer-clear! ctx))
    ctx))

;;; port.scm ends here.

(define-module (smc context port)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc context context)
  #:export (<port-context>
            context-port
            context-counter
            context-counter-set!
            context-counter++!
            context-stanza
            context-stanza-set!
            context-stanza-add!
            context-stanza-clear!
            context-buffer
            context-buffer-set!
            context-buffer-add!
            context-buffer-clear!
            context-clear!
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
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  ;; The stanza holds a logical unit of parsing (e.g. a key/value pair)
  ;;
  ;;<list>
  (stanza
   #:init-value '()
   #:getter     context-stanza
   #:setter     context-stanza-set!))



(define-method (context-counter++! (ctx <port-context>))
  (context-counter-set! ctx (+ (context-counter ctx) 1)))



(define-method (context-buffer-clear! (ctx <context>))
  (context-buffer-set! ctx '()))

(define-method (context-buffer-add! (ctx <context>) value)
  (context-buffer-set! ctx
                       (cons value (context-buffer ctx))))

(define-method (context-stanza-clear! (ctx <context>))
  (context-stanza-set! ctx '()))

(define-method (context-stanza-add! (ctx <context>) value)
  (context-stanza-set! ctx
                       (cons value (context-stanza ctx))))

(define-method (context-clear! (ctx <context>))
  (context-buffer-clear! ctx)
  (context-stanza-clear! ctx))



(define (action:store ctx event)
  (when (context-debug-mode? ctx)
    (log-debug "action:store: event: ~a; buffer: ~a"
               event (context-buffer ctx)))
  (context-buffer-add! ctx event)
  ctx)

(define (action:clear-buffer ctx event)
  "Clear the context CTX buffer."
  (context-buffer-clear! ctx)
  ctx)

(define (action:update-stanza ctx event)
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
(define-module (smc core context)
  #:use-module (oop goops)
  #:use-module (smc core log)
  #:use-module (smc core stack)
  #:export (<context>
            context-debug-mode?
            context-debug-mode-set!
            context-stanza
            context-stanza-set!
            context-stanza-clear!
            context-buffer
            context-buffer-set!
            context-buffer-clear!
            action:store
            action:update-stanza))

;; This class describes a generic parser context.
(define-class <context> ()
  ;; <boolean>
  (debug-mode?
   #:init-value #f
   #:init-keyword #:debug-mode?
   #:getter       context-debug-mode?
   #:setter       context-debug-mode-set!)

  ;; The buffer holds read symbols.
  ;;
  ;; <stack>
  (buffer
   #:init-value (make <stack>)
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  ;; The stanza holds a logical unit of parsing (e.g. a key/value pair)
  ;;
  ;;<stack>
  (stanza
   #:init-value (make <stack>)
   #:getter     context-stanza
   #:setter     context-stanza-set!))



(define-method (context-buffer-clear! (ctx <context>))
  (stack-clear! (context-buffer ctx)))

(define-method (context-stanza-clear! (ctx <context>))
  (stack-clear! (context-stanza ctx)))



(define-method (action:store event (ctx <context>))
  (when (context-debug-mode? ctx)
    (log-debug "action:store: event: ~a; buffer: ~a"
               event (context-buffer ctx)))
  (stack-push! (context-buffer ctx) event)
  ctx)

(define-method (action:update-stanza event (ctx <context>))
  (let ((buf    (context-buffer ctx))
        (stanza (context-stanza ctx)))
    (unless (null? buf)
      (when (context-debug-mode? ctx)
        (log-debug "action:update-stanza: event: ~a; buffer: ~a; stanza: ~a"
                   event
                   (stack-content/reversed buf)
                   (stack-content/reversed stanza)))
      (stack-push! stanza (stack-content/reversed buf))
      (stack-clear! buf))
    ctx))


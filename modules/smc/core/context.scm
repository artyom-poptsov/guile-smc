(define-module (smc core context)
  #:use-module (oop goops)
  #:export (<context>
            context-stanza
            context-stanza-set!
            context-buffer
            context-buffer-set!))

;; This class describes a generic parser context.
(define-class <context> ()
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

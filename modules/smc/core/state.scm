(define-module (smc core state)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (smc core log)
  #:export (<state>
            state-name
            state-run
            state-transition-add!
            state-transitions))


;; This class describes an FSM state.
(define-class <state> ()
  ;; REQUIRED. This slot contains the state name as a <symbol>.
  ;;
  ;; <symbol>
  (name
   #:accessor     state-name
   #:init-value   #f
   #:init-keyword #:name)

  ;; <list> of transitions.
  (transitions
   #:getter       state-transitions
   #:setter       state-transitions-set!
   #:init-value   '()
   #:init-keyword #:transitions))



(define-method (state-transition-add! (self       <state>)
                                      (tguard     <procedure>)
                                      next-state)
  (state-transitions-set! self (append (state-transitions self)
                                       (list (cons tguard next-state)))))



;; Returns two values: next state (or #f) and new context.
(define-method (state-run (self <state>) event context)
  (let loop ((transition-alist (state-transitions self)))
    (if (null? transition-alist)
        (values #f context)
        (let* ((transition (car transition-alist))
               (next-state (cdr transition))
               (new-context ((car transition) event context)))
          ;; (log-debug "[~a] new context: ~a"
          ;;            (state-name self)
          ;;            new-context)
          (cond
           ((not new-context)
            (loop (cdr transition-alist)))
           (else
            ;; (log-debug "[~a] next state: ~a"
            ;;            (state-name self)
            ;;            (if next-state
            ;;                (state-name next-state)
            ;;                #f))
            (values next-state new-context)))))))



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
                                      (action     <procedure>)
                                      next-state)
  (state-transitions-set! self (append (state-transitions self)
                                       (list (list tguard action next-state)))))



;; Returns two values: next state (or #f) and new context.
(define-method (state-run (self <state>) event context)
  (let loop ((transition-alist (state-transitions self)))
    (if (null? transition-alist)
        (values #f context)
        (let* ((transition (car transition-alist))
               (tguard     (list-ref transition 0))
               (action     (list-ref transition 1))
               (next-state (list-ref transition 2)))
          (if (tguard event context)
              (values next-state (action event context))
              (loop (cdr transition-alist)))))))



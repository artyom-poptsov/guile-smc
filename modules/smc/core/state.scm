(define-module (smc core state)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (smc core log)
  #:export (<state>
            state?
            state-name
            state-run
	    state-description
	    state-description-set!
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

  ;; <string>
  (description
   #:getter       state-description
   #:setter       state-description-set!
   #:init-keyword #:description
   #:init-value   #f)

  ;; <list> of transitions.
  (transitions
   #:getter       state-transitions
   #:setter       state-transitions-set!
   #:init-value   '()
   #:init-keyword #:transitions))



(define-method (display (self <state>) (port <port>))
  (next-method)
  (format port "#<state ~a~a ~a>"
          (state-name self)
	  (if (state-description self)
	      (string-append ": " (state-description self))
	      "")
          (number->string (object-address self) 16)))

(define-method (write (self <state>) (port <port>))
  (next-method)
  (display self port))

(define-method (display (self <state>))
  (next-method)
  (display self (current-output-port)))

(define-method (write (self <state>))
  (next-method)
  (display self (current-output-port)))



(define-method (state? x)
  (is-a? x <state>))



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



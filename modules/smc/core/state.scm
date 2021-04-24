(define-module (smc core state)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 receive)
  #:use-module (smc core log)
  #:export (<state>
            state?
            state-name
            state-run
            state-description
            state-description-set!
            state-transition-count
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
  (format port "#<state ~a~a ~a>"
          (state-name self)
          (if (state-description self)
              (string-append ": " (state-description self))
              "")
          (number->string (object-address self) 16)))

(define-method (write (self <state>) (port <port>))
  (display self))



(define-method (state? x)
  (is-a? x <state>))

(define-method (equal? (state-1 <state>) (state-2 <state>))
  (equal? (state-name state-1) (state-name state-2)))



(define-method (state-transition-add! (self       <state>)
                                      (tguard     <procedure>)
                                      (action     <procedure>)
                                      next-state)
  (state-transitions-set! self (append (state-transitions self)
                                       (list (list tguard action next-state)))))



(define-generic state-transition-count)

(define-method (state-transition-count (self <state>))
  (length (state-transitions self)))

(define-method (state-transition-count (self <state>) to)
  (fold (lambda (tr prev)
          (if (equal? (list-ref tr 2) to)
              (+ prev 1)
              prev))
        0
        (state-transitions self)))



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



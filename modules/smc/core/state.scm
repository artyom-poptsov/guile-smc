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
            state-transition-count/foreign
            state-transition-add!
            state-transitions
            state-transitions-set!
            state-final-transitions
            state-recurrent-links-count
            state-has-final-transitions?
            state-has-recurrent-links?
            state-dead-end?))


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



(define-method (%display (self <state>) (port <port>))
  (format port "#<state ~a~a ~a>"
          (state-name self)
          (if (state-description self)
              (string-append ": " (state-description self))
              "")
          (number->string (object-address self) 16)))

(define-method (display (self <state>) (port <port>))
  (%display self port))

(define-method (write (self <state>) (port <port>))
  (%display self port))



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
  (let ((to-name (if (state? to)
                     (state-name to)
                     to)))
    (fold (lambda (tr prev)
            (let ((state (list-ref tr 2)))
              (if (equal? (if (state? state)
                              (state-name state)
                              state)
                          to-name)
                  (+ prev 1)
                  prev)))
          0
          (state-transitions self))))

(define-method (state-transition-count/foreign (self <state>))
  (- (state-transition-count self)
     (state-recurrent-links-count self)))

;; Returns the number of recurrent links that the state SELF has. A recurrent
;; link is a transition of state to itself.
(define-method (state-recurrent-links-count (self <state>))
  (fold (lambda (tr prev)
          (let ((to (list-ref tr 2)))
            (if (equal? (state-name self) (if (symbol? to)
                                              to
                                              (state-name to)))
                (+ prev 1)
                prev)))
        0
        (state-transitions self)))

;; Check if the state SELF has any recurrent links (that is, transitions to
;; itself.)
(define-method (state-has-recurrent-links? (self <state>))
  (> (state-recurrent-links-count self) 0))

;; Get the number of final transitions for a state SELF.
(define-method (state-final-transitions (self <state>))
  (fold (lambda (tr prev)
          (if (list-ref tr 2)
              prev
              (+ prev 1)))
        0
        (state-transitions self)))

;; Check if a state SELF has any final transitions.
(define-method (state-has-final-transitions? (self <state>))
  (> (state-final-transitions self) 0))

;; Check if a state SELF is a dead-end state. A state is considered a dead-end
;; if it has no foreign transitions, has recurrent links and has no final
;; transitions.
(define-method (state-dead-end? (self <state>))
  (and (not (state-has-final-transitions? self))
       (> (state-recurrent-links-count self) 0)
       (zero? (state-transition-count/foreign self))))



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



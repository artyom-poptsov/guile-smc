(define-module (smc fsm)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (smc core log)
  #:use-module (smc core state)
  #:export (<fsm>
            fsm-transition-table
            fsm-transition-add!
            fsm-state-add!
            fsm-state
            fsm-current-state
            fsm-current-state-set!
            fsm-run!

            action:no-op))


(define-class <fsm> ()
  (debug-mode?
   #:init-value   #f
   #:init-keyword #:debug-mode?
   #:getter       fsm-debug-mode?)

  ;; <hash-table>
  (transition-table
   #:getter     fsm-transition-table
   #:setter     fsm-transition-table-set!
   #:init-keyword #:transition-table
   #:init-value (make-hash-table))

  ;; REQUIRED.  Name of the current state.
  ;;
  ;; <state>
  (current-state
   #:init-keyword #:current-state
   #:init-value   #f
   #:getter       fsm-current-state
   #:setter       fsm-current-state-set!))



;; Add a new state to the SM table.
(define-method (fsm-state-add! (self  <fsm>)
                               (state <state>))
  (hash-set! (fsm-transition-table self)
             (state-name state)
             state))

(define-method (fsm-state (self <fsm>)
                          (name <symbol>))
  (hash-ref (fsm-transition-table self) name))



;; Convert a TRANSITION-LIST to a hash table.
(define-method (transition-list->hash-table (transition-list <list>))
  (let ((table (make-hash-table)))
    (for-each (lambda (transition)
                (let* ((state-name (car transition))
                       (state      (hash-ref table state-name)))
                  (when state
                    (error "Duplicate state" state-name))
                  (format #t "tr: ~a~%" (cdr transition))
                  (hash-set! table
                             state-name
                             (make <state>
                               #:name state-name
                               #:transitions (cdr transition)))))
              transition-list)
    table))

(define-method (initialize (self <fsm>) initargs)
  (next-method)
  (let ((states (and (memq #:states initargs)
                     (cadr (memq #:states initargs)))))

    (when states
      (for-each (lambda (state)
                  (fsm-state-add! self state))
                states)
      (fsm-current-state-set! self (car states))))

  (let ((table (and (memq #:transition-table initargs)
                    (cadr (memq #:transition-table initargs)))))
    (when table
      (cond
       ((list? table)
        (fsm-transition-table-set! self (transition-list->hash-table table))
        (fsm-current-state-set! self (fsm-state self (caar table))))
       (else
        (error "Transition table must be a list"))))))



(define-generic fsm-transition-add!)

(define-method (fsm-transition-add! (self       <fsm>)
                                    (state-name <symbol>)
                                    (tguard     <procedure>)
                                    (action     <procedure>)
                                    next-state)
  (state-transition-add! (fsm-state self state-name)
                         tguard
                         action
                         (and next-state
                              (fsm-state self next-state))))

(define-method (fsm-transition-add! (self       <fsm>)
                                    (state-name <symbol>)
                                    (transitions <list>))
  (for-each (lambda (transition)
              (fsm-transition-add! self
                                   state-name
                                   (list-ref transition 0)
                                   (list-ref transition 1)
                                   (list-ref transition 2)))
            transitions))



(define-method (log-debug-transition (from <state>) (to <state>))
  (log-debug "[~a] -> [~a]" (state-name from) (state-name to)))

(define-method (log-debug-transition (from <state>) (to <symbol>))
  (log-debug "[~a] -> [~a]" (state-name from) to))

(define-method (log-debug-transition (from <state>) (to <boolean>))
  (log-debug "[~a] -> [*]" (state-name from)))



(define-method (fsm-run! (self <fsm>) event context)
  (let ((state (fsm-current-state self)))
    (if state
        (let ((state (if (symbol? state)
                         (fsm-state self state)
                         state)))
          (receive (next-state new-context)
              (state-run state event context)
            (when (fsm-debug-mode? self)
              (log-debug-transition state next-state))
            (fsm-current-state-set! self next-state)
            (values next-state new-context)))
        (values #f context))))



(define (action:no-op event ctx)
  ctx)



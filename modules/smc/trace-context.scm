(define-module (smc trace-context)
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (smc core common)
  #:use-module (smc context char)
  #:re-export (guard:#t
               guard:eof-object?
               action:no-op)
  #:export (<trace-context>
            trace-context-previous-entry
            trace-context-current-entry
            trace-context-result
            trace-context-output-port

            event-source
            guard:transition?
            guard:message?
            guard:state-changed?
            action:parse-transition
            action:parse-message
            action:format-error
            print-event
            print-startuml
            print-enduml

            <log-entry>
            <log-entry:message>
            log-entry-timestamp
            log-entry-message
            log-entry?
            log-entry:message?
            log-entry:transition?
            log-entry-timestamp-usec
            log-entry-timestamp-string
            log-entry-timestamp-relative

            <log-entry:transition>
            log-entry-transition-from
            log-entry-transition-to))



;; Example:
;; 2022-01-05 10:58:29.086213 (DEBUG): [read_section_content] -> [read_comment]
(define %fsm-transition-regex
  ;;            1. year      month    day      time (HH:MM:SS)             2. us     3. level      4. from      5. to
  (make-regexp "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})\\.([0-9]+) \\((.+)\\): \\[(.+)\\] -> \\[(.+)\\]"))

;; Example:
;; 2022-01-05 10:58:29.086213 (DEBUG): Some message.
(define %fsm-message-regex
  ;;            1. year      month    day      time (HH:MM:SS)             2. us     2. level  4. message
  (make-regexp "([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})\\.([0-9]+) \\((.+)\\): (.*)"))



(define-class-with-docs <trace-context> (<context>)
  "This class describes a trace context that is used to parse FSM logs."

  (port
   #:init-value   (current-input-port)
   #:init-keyword #:port
   #:getter       trace-context-port)

  ;; <number>
  (line-number
   #:init-value  0
   #:getter      trace-context-line-number
   #:setter      trace-context-line-number-set!)

  ;; <log-entry>
  (first-entry
   #:init-value  #f
   #:getter      trace-context-first-entry
   #:setter      trace-context-first-entry-set!)

  ;; <log-entry>
  (previous-entry
   #:init-value  #f
   #:getter      trace-context-previous-entry
   #:setter      trace-context-previous-entry-set!)

  ;; <log-entry>
  (current-entry
   #:init-value  #f
   #:getter      trace-context-current-entry
   #:setter      trace-context-current-entry-set!)

  ;; <list>
  (result
   #:init-value  '()
   #:getter      trace-context-result
   #:setter      trace-context-result-set!))

(define-class <log-entry> ()
  ;; <symbol>
  (level
   #:init-value   #f
   #:init-keyword #:level
   #:getter       log-entry-level)

  (timestamp
   #:init-keyword #:timestamp
   #:init-value   #f
   #:getter       log-entry-timestamp)

  ;; <string>
  (timestamp-string
   #:init-keyword #:timestamp-string
   #:init-value   #f
   #:getter       log-entry-timestamp-string)

  ;; Timestamp in microseconds.
  ;;
  ;; <number>
  (timestamp-usec
   #:init-keyword #:timestamp-usec
   #:init-value   #f
   #:getter       log-entry-timestamp-usec)

  ;; Timestamp in microseconds relative to the first log entry.
  ;;
  ;; <number>
  (timestamp-relative
   #:init-keyword #:timestamp-relative
   #:init-value   #f
   #:getter       log-entry-timestamp-relative))

(define-class <log-entry:transition> (<log-entry>)
  (transition-from
   #:init-keyword #:transition-from
   #:init-value   #f
   #:getter       log-entry-transition-from)

  (transition-to
   #:init-keyword #:transition-to
   #:init-value   #f
   #:getter       log-entry-transition-to))

(define-class <log-entry:message> (<log-entry>)
  ;; <string>
  (message
   #:init-value   #f
   #:init-keyword #:message
   #:getter       log-entry-message))

(define (log-entry? x)
  (is-a? x <log-entry>))

(define (log-entry:transition? x)
  (is-a? x <log-entry:transition>))

(define (log-entry:message? x)
  (is-a? x <log-entry:message>))




(define-method (%display (log-entry <log-entry>) (port <port>))
  (format port "#<log-entry ~a ~a ~a>"
          (log-entry-timestamp log-entry)
          (log-entry-level log-entry)
          (object-address/hex-string log-entry)))

(define-method (display (log-entry <log-entry>) (port <port>))
  (%display log-entry port))

(define-method (write (log-entry <log-entry>) (port <port>))
  (%display log-entry port))

(define-method (%display (log-entry <log-entry:transition>) (port <port>))
  (format port "#<log-entry:transition ~a ~a [~a] -> [~a] ~a>"
          (log-entry-timestamp-string log-entry)
          (log-entry-level log-entry)
          (log-entry-transition-from log-entry)
          (log-entry-transition-to log-entry)
          (object-address/hex-string log-entry)))

(define-method (display (log-entry <log-entry:transition>) (port <port>))
  (%display log-entry port))

(define-method (write (log-entry <log-entry:transition>) (port <port>))
  (%display log-entry port))

(define-method (%display (log-entry <log-entry:message>) (port <port>))
  (format port "#<log-entry:message ~a ~a ~a ~a>"
          (log-entry-timestamp-string log-entry)
          (log-entry-level log-entry)
          (log-entry-message log-entry)
          (object-address/hex-string log-entry)))

(define-method (display (log-entry <log-entry:message>) (port <port>))
  (%display log-entry port))

(define-method (write (log-entry <log-entry:message>) (port <port>))
  (%display log-entry port))


(define-method (%line-number++! ctx)
  (trace-context-line-number-set! ctx
                                  (+ (trace-context-line-number ctx) 1)))

(define-method (trace-context-add-entry! ctx entry)
  (trace-context-result-set! ctx
                             (cons entry (trace-context-result ctx))))



(define-method (event-source (ctx <trace-context>))
  (%line-number++! ctx)
  (read-line (trace-context-port ctx)))



(define (guard:transition? ctx line)
  (not (equal? (regexp-exec %fsm-transition-regex line) #f)))

(define (guard:message? ctx line)
  (not (equal? (regexp-exec %fsm-message-regex line) #f)))



(define (trace-context-error ctx line message)
  (throw 'trace-context-error
         (format #f "~a: ~a: ~a: ~a"
                 (trace-context-port ctx)
                 (trace-context-line-number ctx)
                 line
                 message)))

(define (action:format-error ctx line)
  (trace-context-error ctx line "Format error"))

(define (parse-timestamp ts)
  (strptime "%Y-%m-%d %H:%M:%S" ts))

(define (%trace-context-update! ctx entry)
  (unless (trace-context-first-entry ctx)
    (trace-context-first-entry-set! ctx entry))

  (trace-context-previous-entry-set! ctx (trace-context-current-entry ctx))
  (trace-context-current-entry-set! ctx entry)

  (trace-context-add-entry! ctx entry)
  ctx)

(define (action:parse-transition ctx line)
  (let* ((m (regexp-exec %fsm-transition-regex line))
         (timestamp:tm        (parse-timestamp (match:substring m 1)))
         (timestamp:s         (- (string->number (strftime "%s" (car timestamp:tm)))))
         (timestamp:usec-part (string->number (match:substring m 2)))
         (timestamp:usec      (+ (* timestamp:s 1000000)
                                 timestamp:usec-part))
         (entry (make <log-entry:transition>
                  #:timestamp          timestamp:tm
                  #:timestamp-string   (match:substring m 1)
                  #:timestamp-usec     timestamp:usec
                  #:timestamp-relative (if (trace-context-first-entry ctx)
                                           (- timestamp:usec
                                              (log-entry-timestamp-usec
                                               (trace-context-first-entry ctx)))
                                           0)
                  #:level            (string->symbol (match:substring m 3))
                  #:transition-from  (match:substring m 4)
                  #:transition-to    (match:substring m 5))))
    (%trace-context-update! ctx entry)))


(define (action:parse-message ctx line)
  (let* ((m (regexp-exec %fsm-message-regex line))
         (timestamp:tm        (parse-timestamp (match:substring m 1)))
         (timestamp:s         (- (string->number (strftime "%s" (car timestamp:tm)))))
         (timestamp:usec-part (string->number (match:substring m 2)))
         (timestamp:usec      (+ (* timestamp:s 1000000)
                                 timestamp:usec-part))
         (entry (make <log-entry:message>
                  #:timestamp          timestamp:tm
                  #:timestamp-string   (match:substring m 1)
                  #:timestamp-usec     timestamp:usec
                  #:timestamp-relative (if (trace-context-first-entry ctx)
                                           (- timestamp:usec
                                              (log-entry-timestamp-usec
                                               (trace-context-first-entry ctx)))
                                           0)
                  #:level            (string->symbol (match:substring m 3))
                  #:message          (match:substring m 4))))
    (%trace-context-update! ctx entry)))

;;; trace-context.scm ends here.

(define-module (smc puml)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (smc core state)
  #:use-module (smc fsm)
  #:use-module (smc core log)
  #:use-module (smc guards char)
  #:export (<parser>
            puml->fsm
            parser-run))



(define-class <context> ()
  (fsm
   #:init-value (make <fsm>)
   #:getter     context-fsm)

  (buffer
   #:init-value '()
   #:getter     context-buffer
   #:setter     context-buffer-set!)

  (stanza
   #:init-value '()
   #:getter     context-stanza
   #:setter     context-stanza-set!))


(define (buffer->string buffer)
  (list->string (reverse buffer)))

(define (state-name-symbol? char)
  (or (char-set-contains? char char-set:letter)
      (char-set-contains? char char-set:digit)
      (char=? #\_)))

(define (guard:square-bracket? ch ctx)
  (char=? ch #\[))

(define (guard:at-symbol? ch ctx)
  (char=? ch #\@))

(define (guard:star? ch ctx)
  (char=? ch #\* ))

(define (guard:dash? ch ctx)
  (char=? ch #\-))

(define (guard:arrow-left-end? ch ctx)
  (char=? ch #\<))

(define (guard:arrow-right-end? ch ctx)
  (char=? ch #\>))



(define (action:store-symbol ch ctx)
  (context-buffer-set! ctx (cons ch (context-buffer ctx)))
  ctx)

(define (action:add-state ch ctx)
  (let ((fsm (context-fsm ctx))
        (buf (context-buffer ctx)))
    (log-debug "action:add-state: buffer: ~a" buf)
    (let ((state (make <state>
                   #:name (list->symbol (reverse buf)))))
      (log-debug "action:add-state: state: ~a" state)
      (unless (fsm-current-state fsm)
        (fsm-current-state-set! fsm state))
      (fsm-state-add! fsm state)
      (context-buffer-set! ctx '())
      ctx)))

(define (action:add-buffer-to-stanza ch ctx)
  (let ((fsm (context-fsm ctx))
        (buf (context-buffer ctx)))
    (log-debug "action:add-buffer-to-stanza: buffer: ~a" buf)
    (log-debug "action:add-buffer-to-stanza: stanza: ~a" (context-stanza ctx))
    (context-stanza-set! ctx (append (context-stanza ctx)
                                     (list (list->symbol (reverse buf)))))
    (log-debug "action:add-buffer-to-stanza: stanza: ~a" (context-stanza ctx))
    (context-buffer-set! ctx '())

    ctx))

(define (action:add-state-with-transition-to ch ctx)
  (let ((fsm    (context-fsm ctx))
        (buf    (context-buffer ctx)))
    (log-debug "action:add-state-with-transition-to: buffer: ~a"
               buf)
    (log-debug "action:add-state-with-transition-to: stanza: ~a"
               (context-stanza ctx))
    (action:add-buffer-to-stanza ch ctx)
    (log-debug "action:add-state-with-transition-to: buffer: ~a"
               buf)
    (let* ((stanza (context-stanza ctx))
           (from (list-ref stanza 0))
           (to   (list-ref stanza 1)))
      (log-debug "action:add-state-with-transition-to: stanza: ~a" stanza)
      (log-debug "action:add-state-with-transition-to: [~a] -> [~a]" from to)
      (context-stanza-set! ctx '())
      (let ((state-from (make <state> #:name from))
            (state-to   (make <state> #:name to)))
        (fsm-state-add! fsm state-from)
        (log-debug "action:add-state-with-transition-to: state: ~a" state-from)
        (log-debug "action:add-state-with-transition-to: fsm: ~a" fsm)
        (fsm-transition-add! fsm from
                             (list (list (const #t) action:no-op state-to)))
        ctx))))

(define (action:check-start-tag ch ctx)
  (let* ((buf (context-buffer ctx))
         (str (list->string (reverse buf))))
    (unless (string=? str "@startuml")
      (error "Misspelled @startuml" str))
    (context-buffer-set! ctx '())
    ctx))

(define (action:no-start-tag-error ch ctx)
  (error "No start tag found"))



(define (puml->fsm port)
  (log-use-stderr! #t)
  (let ((reader-fsm
         (make <fsm>
           #:debug-mode? #t
           #:transition-table
           `((search-start-tag
              (,guard:eof-object?     ,action:no-op              #f)
              (,guard:at-symbol?      ,action:store-symbol       read-start-tag)
              (,guard:single-quote?   ,action:no-op              search-start-tag/skip-comment)
              (,guard:letter?         ,action:no-start-tag-error #f)
              (,guard:#t              ,action:no-op              search-start-tag))
             (search-start-tag/skip-comment
              (,guard:eof-object?     ,action:no-op              #f)
              (,guard:newline?        ,action:no-op              search-start-tag)
              (,guard:#t              ,action:no-op              search-start-tag/skip-comment))
             (read
              (,guard:eof-object?     ,action:no-op              #f)
              (,guard:single-quote?   ,action:no-op        read/skip-comment)
              (,guard:square-bracket? ,action:no-op        read-square-brackets)
              (,guard:letter?         ,action:store-symbol read-state)
              (,guard:#t              ,action:no-op        read))
             (read/skip-comment
              (,guard:eof-object?     ,action:no-op              #f)
              (,guard:newline?        ,action:no-op              read)
              (,guard:#t              ,action:no-op              read/skip-comment))
             (read-start-tag
              (,guard:eof-object?     ,action:no-op           #f)
              (,guard:space?          ,action:check-start-tag read)
              (,guard:newline?        ,action:check-start-tag read)
              (,guard:#t              ,action:store-symbol    read-start-tag))
             (read-square-brackets
              (,guard:eof-object?     ,action:no-op        #f)
              (,guard:star?           ,action:no-op        search-entry-point)
              (,guard:#t              ,action:no-op        read))
             (search-entry-point
              (,guard:eof-object?     ,action:no-op        #f)
              (,guard:letter?         ,action:store-symbol read-entry-point)
              (,guard:#t              ,action:no-op        search-entry-point))
             (read-entry-point
              (,guard:eof-object?     ,action:no-op        #f)
              (,guard:space?          ,action:add-state    read)
              (,guard:newline?        ,action:add-state    read)
              (,guard:#t              ,action:store-symbol read-entry-point))
             (read-state
              (,guard:eof-object?     ,action:no-op                #f)
              (,guard:space?          ,action:add-buffer-to-stanza search-state-transition)
              (,guard:#t              ,action:store-symbol         read-state))
             (search-state-transition
              (,guard:eof-object?     ,action:no-op        #f)
              (,guard:dash?           ,action:no-op        read-state-right-arrow)
              (,guard:arrow-left-end? ,action:no-op        read-state-left-arrow)
              (,guard:#t              ,action:no-op        search-state-transition))
             (read-state-right-arrow
              (,guard:eof-object?     ,action:no-op        #f)
              (,guard:space?          ,action:no-op        search-state-transition-to)
              (,guard:#t              ,action:no-op        read-state-right-arrow))
             (search-state-transition-to
              (,guard:eof-object?     ,action:no-op        #f)
              (,guard:letter?         ,action:store-symbol read-state-transition-to)
              (,guard:#t              ,action:no-op        search-state-transition-to))
             (read-state-transition-to
              (,guard:eof-object?     ,action:no-op                        #f)
              (,guard:newline?        ,action:add-state-with-transition-to read)
              (,guard:#t              ,action:store-symbol                 read-state-transition-to))))))

    (let loop ((context (make <context>)))
      (receive (new-state new-context)
          (fsm-run! reader-fsm (get-char port) context)
        (if new-state
            (loop new-context)
            (context-fsm context))))))


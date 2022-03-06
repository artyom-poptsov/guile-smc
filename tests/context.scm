(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (smc context context)
             (smc context char-context))


(define %test-suite-name "context")

(test-begin %test-suite-name)


;;; Guards.

(test-assert "guard:#t"
  (guard:#t 'context 'event))

(test-assert "guard:newline?"
  (guard:newline? '() #\newline))

(test-assert "guard:space?"
  (guard:space? '() #\space))

(test-assert "guard:letter?"
  (guard:letter? '() #\a))

(test-assert "guard:single-quote?"
  (guard:single-quote? '() #\'))

(test-assert "guard:colon?"
  (guard:colon? '() #\:))

(test-assert "guard:left-square-bracket?"
  (guard:left-square-bracket? '() #\[))

(test-assert "guard:right-square-bracket?"
  (guard:right-square-bracket? '() #\]))

(test-assert "guard:at-symbol?"
  (guard:at-symbol? '() #\@))


;;; Generic context.

(test-assert "context?"
  (and (context? (make <context>))
       (not (context? "not a context"))))

(test-assert "<context>: buffer and stanza initially must be empty lists"
  (let ((ctx (make <context>)))
    (and (list? (context-buffer ctx))
         (null? (context-buffer ctx))
         (list? (context-stanza ctx))
         (null? (context-stanza ctx)))))


(test-equal "context-buffer-add!"
  '(3 2 1)
  (let ((ctx (make <context>)))
    (context-buffer-add! ctx 1)
    (context-buffer-add! ctx 2)
    (context-buffer-add! ctx 3)
    (context-buffer ctx)))

(test-equal "context-stanza-add!"
  '("hello" "world")
  (let ((ctx (make <context>)))
    (context-stanza-add! ctx "world")
    (context-stanza-add! ctx "hello")
    (context-stanza ctx)))


(test-equal "action:store"
  '("world" "hello")
  (let ((ctx (make <context>)))
    (action:store (action:store ctx "hello") "world")
    (context-buffer ctx)))

(test-assert "action:update-stanza"
  (let ((ctx (make <context>)))
    (action:store (action:store ctx "hello") "world")
    (action:update-stanza ctx #f)
    (and (equal? (context-stanza ctx)
                 '(("hello" "world")))
         (null? (context-buffer ctx)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


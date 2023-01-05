(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (tests common)
             (smc context context)
             (smc context port)
             (smc context char))


(define %test-suite-name "context")

(configure-test-logging! %test-suite-name)
(test-begin %test-suite-name)


;;; Guards.

(test-assert "guard:#t"
  (guard:#t 'context 'event))

(test-assert "guard:newline?"
  (guard:newline? (make <char-context>) #\newline))

(test-assert "guard:space?"
  (guard:space? (make <char-context>) #\space))

(test-assert "guard:letter?"
  (guard:letter? (make <char-context>) #\a))

(test-assert "guard:single-quote?"
  (guard:single-quote? (make <char-context>) #\'))

(test-assert "guard:colon?"
  (guard:colon? (make <char-context>) #\:))

(test-assert "guard:left-square-bracket?"
  (guard:left-square-bracket? (make <char-context>) #\[))

(test-assert "guard:right-square-bracket?"
  (guard:right-square-bracket? (make <char-context>) #\]))

(test-assert "guard:at-symbol?"
  (guard:at-symbol? (make <char-context>) #\@))


;;; Generic context.

(test-assert "context?"
  (and (context? (make <context>))
       (not (context? "not a context"))))

(test-assert "<context>: buffer and stanza initially must be empty lists"
  (let ((ctx (make <port-context>)))
    (and (list? (context-buffer ctx))
         (null? (context-buffer ctx))
         (list? (context-stanza ctx))
         (null? (context-stanza ctx)))))


(test-equal "context-buffer-add!"
  '(3 2 1)
  (let ((ctx (make <port-context>)))
    (context-buffer-add! ctx 1)
    (context-buffer-add! ctx 2)
    (context-buffer-add! ctx 3)
    (context-buffer ctx)))

(test-equal "context-stanza-add!"
  '("hello" "world")
  (let ((ctx (make <port-context>)))
    (context-stanza-add! ctx "world")
    (context-stanza-add! ctx "hello")
    (context-stanza ctx)))


(test-equal "action:store"
  '("world" "hello")
  (let ((ctx (make <port-context>)))
    (action:store (action:store ctx "hello") "world")
    (context-buffer ctx)))

(test-assert "action:update-stanza"
  (let ((ctx (make <port-context>)))
    (action:store (action:store ctx "hello") "world")
    (action:update-stanza ctx #f)
    (and (equal? (context-stanza ctx)
                 '(("hello" "world")))
         (null? (context-buffer ctx)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


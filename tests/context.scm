(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             ((smc core stack)
              #:prefix smc:)
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

(test-assert "<context>: buffer and stanza initially must be empty stacks"
  (let ((ctx (make <context>)))
    (and (smc:stack?       (context-buffer ctx))
         (smc:stack-empty? (context-buffer ctx))
         (smc:stack?       (context-stanza ctx))
         (smc:stack-empty? (context-stanza ctx)))))

(test-equal "action:store"
  '("world" "hello")
  (let ((ctx (make <context>)))
    (action:store (action:store ctx "hello") "world")
    (smc:stack-content (context-buffer ctx))))

(test-assert "action:update-stanza"
  (let ((ctx (make <context>)))
    (action:store (action:store ctx "hello") "world")
    (action:update-stanza ctx #f)
    (and (equal? (smc:stack-content (context-stanza ctx))
                 '(("hello" "world")))
         (smc:stack-empty? (context-buffer ctx)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)


(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (oop goops)
             (smc core set))


(define %test-name "set")
(test-begin %test-name)



(test-equal "initialize: empty set"
  '()
  (let ((set (make <set>)))
    (set-content set)))

(test-equal "set-add!"
  '(a)
  (let ((set (make <set>)))
    (set-add! set 'a)
    (set-content set)))

(test-equal "set-remove!"
  '(c)
  (let ((set (make <set>)))
    (set-add! set 'a)
    (set-add! set 'b)
    (set-add! set 'c)
    (set-remove! set 'a)
    (set-remove! set 'b)
    (set-content set)))

(test-assert "set-empty? #t"
  (set-empty? (make <set>)))

(test-assert "set-empty? #f"
  (let ((set (make <set>)))
    (set-add! set 'a)
    (not (set-empty? set))))

(test-equal "set-capacity"
  3
  (let ((set (make <set>)))
    (set-add! set 'a)
    (set-add! set 'b)
    (set-add! set 'c)
    (set-capacity set)))

(test-assert "set-contains? #t"
  (let ((set (make <set>)))
    (set-add! set 'a)
    (set-add! set 'b)
    (set-add! set 'c)
    (set-contains? set 'a)))

(test-equal "set-contains? #f"
  #f
  (let ((set (make <set>)))
    (set-add! set 'b)
    (set-add! set 'c)
    (set-contains? set 'a)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

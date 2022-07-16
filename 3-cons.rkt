#lang racket
(define cal (lambda (op a b) (
  cond
  ((eq? op `+) (+ a b))
  ((eq? op `-) (- a b))
  ((eq? op `*) (* a b))
  ((eq? op `/) (/ a b))
  (else (error "Unknown operator" op))
)))
(cal `+ 1 2)
(cal `- 1 2)
(cal `* 1 2)
(cal `/ 1 2)
(define rember (lambda (l a) (
  cond
  ((eq? (car l) a) (cdr l))
  (else (
    let (
      (head (car l))
      (tail (rember (cdr l) a))
    ) (cons head tail)
  ))
)))
(rember `(foo bar) `foo)
(rember `(foo bar) `bar)
(rember `(foo bar baz) `bar)

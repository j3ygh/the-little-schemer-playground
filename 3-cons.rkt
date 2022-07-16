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
  ((null? l) `())
  ((eq? (car l) a) (cdr l))
  (else (
    cons
    (car l)
    (rember (cdr l) a)
  ))
)))

(rember `(foo bar) `foo)
(rember `(foo bar) `bar)
(rember `(foo bar baz) `bar)
(rember `() `bar)

(define first (lambda (l) (
  cond
  ((null? l) `())
  (else (car l))
)))

(first `(a b c))
(first `())

(define firsts (lambda (l) (
  cond
  ((null? l) `())
  (else (
    cons
    (first (car l))
    (firsts (cdr l))
  ))
)))

(firsts `((a b c) (1 2 3) (x y z)))

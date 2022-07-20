#lang racket

(define atom?
  (lambda (x)
    (and
      (not (pair? x))
      (not (null? x)))))

(define rember
  (lambda (l a) 
    (cond
      ((null? l)
        `())
      ((eq? (car l) a)
        (cdr l))
      (else
        (cons (car l)
          (rember (cdr l) a))))))

(let
  ((a `sauce)
  (l `(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))))
    (list a l))

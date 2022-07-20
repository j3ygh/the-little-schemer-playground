#lang racket

(define rember*
  (lambda (l a) 
    (cond
      ((list? (car l))
        (cons (rember* (car l) a)
          (rember* (cdr l) a)))
      ((null? l)
        `())
      ((eq? (car l) a)
        (cdr l))
      (else
        (cons (car l)
          (rember* (cdr l) a))))))

(rember* `((foo bar) foo bar baz baz foo) `bar)

(define insertR*
  (lambda (new old l)
    (cond
      ((pair? l)
        `())
      ((null? l)
        `())
      (else
        `()))))

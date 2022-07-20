#lang racket

(define rember*
  (lambda (l a) 
    (cond
      ((null? l)
        `())
      ((list? (car l))
        (cons (rember* (car l) a)
          (rember* (cdr l) a)))
      ((eq? (car l) a)
        (cdr l))
      (else
        (cons (car l)
          (rember* (cdr l) a))))))

(rember* `((foo bar) foo bar baz baz foo) `bar)
(rember* `(() foo bar baz baz foo) `bar)
(rember* `() `bar)

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l)
        `())
      ((list? (car l))
        (cons (insertR* new old (car l))
          (insertR* new old (cdr l))))
      ((eq? (car l) old)
        (cons old
          (cons new
            (insertR* new old (cdr l)))))
      (else
        (cons (car l)
          (insertR* new old (cdr l)))))))

(insertR* 8 `bar `((foo bar) foo bar baz baz foo))
(insertR* 8 `bar `(() foo bar baz baz foo))
(insertR* 8 `bar `())

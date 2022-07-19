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

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat)
        `())
      ((eq? (car lat) old)
        (cons old (cons new (cdr lat))))
      (else
        (cons (car lat) (insertR new old (cdr lat)))))))

(insertR `z `b `(c c c c b a))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat)
        `())
      ((eq? (car lat) old)
        (cons new lat))
      (else
        (cons (car lat) (insertL new old (cdr lat)))))))

(insertL `z `b `(c c c c b a))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat)
        `())
      ((eq? (car lat) old)
        (cons new (cdr lat)))
      (else
        (cons (car lat) (subst new old (cdr lat)))))))

(subst `z `b `(c c c c b a))

(define include?
  (lambda (lat a)
  (cond
    ((null? lat)
      #f)
    ((eq? (car lat) a)
      #t)
    (else
      (include? (cdr lat) a)))))

(include? `(a b) `a)
(include? `(a b c) `b)
(include? `(a b c) `d)

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat)
        `())
      ((include? (list o1 o2) (car lat))
        (cons new (cdr lat)))
      (else
        (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 `z `x `b `(c c c c b a))
(subst2 `z `c `b `(c c c c b a))

(define multirember
  (lambda (lat a)
    (cond
      ((null? lat)
        `())
      ((eq? (car lat) a)
        (multirember (cdr lat) a))
      (else
        (cons (car lat) (multirember (cdr lat) a))))))

(multirember `(1 1 2 2 3 3) 2)

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat)
        `())
      ((eq? (car lat) old)
        (cons old
          (cons new
            (multiinsertR new old (cdr lat)))))
      (else
        (cons (car lat)
          (multiinsertR new old (cdr lat)))))))

(multiinsertR `fried `fish `(chips and fish or fish and fried))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat)
        `())
      ((eq? (car lat) old)
        (cons new
          (cons old
            (multiinsertL new old (cdr lat)))))
      (else
        (cons (car lat)
          (multiinsertL new old (cdr lat)))))))

(multiinsertL `fried `fish `(chips and fish or fish and fried))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat)
        `())
      ((eq? (car lat) old)
        (cons new
          (multisubst new old (cdr lat))))
      (else
        (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst `fried `fish `(chips and fish or fish and fried))

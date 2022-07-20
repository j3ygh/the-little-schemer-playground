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

; The First Commandment
; (final version)
; When recurring on a list of atoms, lat, ask two questions
; about it: (null? lat) and else.
; When recurring on a number, n, ask two questions about
; it: (zero ? n) and else.
; When recurring on a list of S-expressions, l, ask three
; question about it: (null? l), ( atom? ( car l)), and else. 

; Because all *-functions work on lists that are
; either
; - empty,
; - an atom consed onto a list, or
; - a list consed onto a list.

; The Fourth Commandment
; (final version)
; Always change at least one argument while recurring.
; When recurring on a list of atoms, lat, use ( cdr lat). When
; recurring on a number, n, use (sub1 n). And when recurring on a list of S-expressions, l, use ( car l) and ( cdr l) if
; neither (null? l) nor ( atom? ( car l)) are true.
; It must be changed to be closer to termination. The changing argument must be tested in the termination condition:
; when using cdr, test termination with null? and
; when using sub1 , test termination with zero ?. 

(define occur*
  (lambda (a l)
    (cond
      ((null? l)
        0)
      ((list? (car l))
        (+ (occur* a (car l))
          (occur* a (cdr l))))
      ((eq? (car l) a)
        (+ 1 (occur* a (cdr l))))
      (else
        (occur* a (cdr l))))))

(occur* `a `((a b c) a b c (a (a (b)))))
(occur* `a `(() a b c (a (a (b)))))
(occur* `a `())

(define subst*
  (lambda (new old l)
    (cond
      ((null? l)
        `())
      ((list? (car l))
        (cons (subst* new old (car l))
          (subst* new old (cdr l))))
      ((eq? (car l) old)
        (cons new
          (subst* new old (cdr l))))
      (else
        (cons (car l)
          (subst* new old (cdr l)))))))

(subst* 6 `a `((a b c) a b c (a (a (b)))))
(subst* 6 `a `(() a b c (a (a (b)))))
(subst* 6 `a `())

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l)
        `())
      ((list? (car l))
        (cons (insertL* new old (car l))
          (insertL* new old (cdr l))))
      ((eq? (car l) old)
        (cons new
          (cons old
            (insertL* new old (cdr l)))))
      (else
        (cons (car l)
          (insertL* new old (cdr l)))))))

(insertL* 6 `a `((a b c) a b c (a (a (b)))))
(insertL* 6 `a `(() a b c (a (a (b)))))
(insertL* 6 `a `())

(define member*
  (lambda (a l)
    (cond
      ((null? l)
        #f)
      ((list? (car l))
        (or (member* a (car l))
          (member* a (cdr l))))
      ((eq? (car l) a)
        #t)
      (else
        (member* a (cdr l))))))

(member* `a `((a b c) a b c (a (a (b)))))
(member* `a `(() a b c (a (a (b)))))
(member* `a `())

; leftmost, page 103
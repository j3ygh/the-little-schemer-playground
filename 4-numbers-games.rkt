#lang racket
; page 74

(define atom?
  (lambda (x)
    (and
     (not (pair? x))
     (not (null? x)))))

(atom? 14)

(+ 46 12)

(define add-one
  (lambda (n)
    (+ n 1)))

(add-one 67)

(define sub-one
  (lambda (n)
    (- n 1)))

(sub-one 5)
(sub-one 0)

(define zero?
  (lambda (n)
    (cond
      ((and (number? n) (= n 0))
       #t)
      (else
       #f))))

(zero? 0)
(zero? 1)
(zero? `abc)
(zero? "foo")
(zero? `())
(zero? `(1 2 3))

(define add
  (lambda (n m)
    (cond
      ((zero? m)
       n)
      (else
       (add (add-one n) (sub-one m))))))

(add 46 12)

(define sub
  (lambda (n m)
    (cond
      ((zero? m)
       n)
      (else
       (sub (sub-one n) (sub-one m))))))

(- 14 3)
(- 17 9)

; The First Commandment
; (first revision)
; When recurring on a list of atoms, lat, ask two questions
; about it: (null? lat) and else.
; When recurring on a number, n, ask two questions about
; it: (zero ? n) and else.

(define addtup
  (lambda (tup)
    (cond
      ((null? tup)
       0)
      (else
       (+ (car tup)
          (addtup (cdr tup)))))))

(addtup `(1 2 3 4 5 6 7 8 9 10))

(define x
  (lambda (n m)
    (cond
      ((zero? m)
       0)
      (else
       (+ n
          (x n (sub-one m)))))))

(x 3 7)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
       `())
      (else
        (cons (+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ `(3 7) `(4 6))

(define tup+2
  (lambda (tup1 tup2)
    (cond
      ((null? tup1)
        tup2)
      ((null? tup2)
        tup1)
      (else
        (cons (+ (car tup1) (car tup2))
          (tup+2 (cdr tup1) (cdr tup2)))))))

(tup+2 `(3 7) `(4 6 8))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (> (sub-one n) (sub-one m))))))

(> 5 3)
(> 3 5)
(> 3 3)

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (< (sub-one n) (sub-one m))))))

(< 5 3)
(< 3 5)
(< 3 3)

(define ==
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(== 3 5)
(== 5 3)
(== 3 3)

(define **
  (lambda (n m)
    (cond
      ((zero? m)
        1)
      (else
        (* n
          (** n (sub-one m)))))))

(** 2 4)

(define //
  (lambda (n m)
    (cond
      ((< n m)
        0)
      (else
        (add1 (// (- n m) m))))))

(// 15 4)

(define length
  (lambda (lat)
    (cond
      ((null? lat)
        0)
      (else
        (add1 (length (cdr lat)))))))

(length `(1 2 3))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub-one n))
        (car lat))
      (else
        (pick (- n 1) (cdr lat))))))

(pick 2 `(foo bar baz))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub-one n))
        (cdr lat))
      (else
        (cons (car lat)
          (rempick (sub-one n) (cdr lat)))))))

(rempick 2 `(foo bar baz))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat)
        `())
      ((number? (car lat))
        (no-nums (cdr lat)))
      (else
        (cons (car lat)
          (no-nums (cdr lat)))))))

(no-nums `(1 a 2 b 3 c))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat)
        `())
      ((number? (car lat))
        (cons (car lat)
          (all-nums (cdr lat))))
      (else
        (all-nums (cdr lat))))))

(all-nums `(1 a 2 b 3 c))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
        (= a1 a2))
      ((or (number? a1) (number? a2))
        #f)
      (else
        (eq? a1 a2)))))

(eqan? `a `a)
(eqan? `a 1)
(eqan? 2 `b)
(eqan? 2 2)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat)
        0)
      ((eq? (car lat) a)
        (+ 1 (occur a (cdr lat))))
      (else
        (occur a (cdr lat))))))

(occur `a `(a b a b))
(occur 1 `(1 2 1 2))

(define one?
  (lambda (n)
    (= n 1)))

(one? 1)
(one? 2)

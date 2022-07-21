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

(define leftmost
  (λ (l)
    (cond
      ((list? (car l))
       (leftmost (car l)))
      (else
       (car l)))))

(leftmost `(1))

; True or false: it is possible that one of the
; arguments of (and . . . ) and (or ... ) is not
; considered? 1

; True, because (and ... ) stops if the first
; argument has the value #f, and (or ... )
; stops if the first argument has the value #t .

; 1 ( cond ... ) also has the property of not considering all of
; its arguments. Because of this property, however, neither
; (and ... ) nor (or ... ) can be deft ned as functions in terms
; of (cond ... ), though both (and ... ) and (or ... ) can be
; expressed as abbreviations of ( cond ... )-expressions:
; (and tt {3) = (cond (tt {3) (else #f))
; and
; (or tt {3) = (cond (tt #t) (else {3))

; Use racket-server formatter and `λ` instead of `lambda`


(define equal?
  (λ (s1 s2)
    (cond
      ((and (list? s1) (list? s2)
            (eqlist? s1 s2)))
      ((or (list? s1) (list? s2))
       #f)
      (else
       (eq? s1 s2)))))


(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2))
       #t)
      ((or (null? l1) (null? l2))
       #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(equal? 1 1)
(equal? `foo `foo)
(equal? `() `())
(equal? `(1 2 3) `(1 2 3))
(equal? `((1 2 3) 2 3) `((1 2 3) 2 3))
(equal? `(() 2 3) `((1 2 3) 2 3))
(equal? `((3 2 1) 2 3) `((1 2 3) 2 3))

(define rember
  (λ (s l)
    (cond
      ((null? l)
       `())
      ((list? (car l))
       (cons (rember s (car l))
             (rember s (cdr l))))
      ((eq? s (car l))
       (rember s (cdr l)))
      (else
       (cons (car l)
             (rember s (cdr l)))))))

(rember `bar `(foo bar baz (foo bar) (bar)))
(rember `foo `(foo bar baz (foo bar) (bar)))

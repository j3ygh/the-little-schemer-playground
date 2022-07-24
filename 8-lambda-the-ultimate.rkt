#lang racket

; Add a `test?` param
(define rember-f-old
  (λ (test? s l)
    (cond
      ((null? l)
       `())
      ((test? s (car l))
       (cdr l))
      (else
       (cons (car l)
             (rember-f-old test? s (cdr l)))))))

(rember-f-old eq? `bar `(foo bar baz (foo bar) (bar)))
(rember-f-old eq? `foo `(foo bar baz (foo bar) (bar)))

(define eq?-c
  (λ (a)
    (λ (x)
      (eq? x a ))))

(define eq?-salad (eq?-c `salad))

(eq?-salad `salad)
(eq?-salad `tuna)

; Make it curry-ing
(define rember-f
  (λ (test?)
    (λ (s l)
      (cond
        ((null? l)
         `())
        ((test? s (car l))
         (cdr l))
        (else
         (cons (car l)
               ((rember-f test?) s (cdr l))))))))

(define rember-eq? (rember-f eq?))

(rember-eq? `bar `(foo bar baz (foo bar) (bar)))
((rember-f eq?) `foo `(foo bar baz (foo bar) (bar)))

(define insertL-f
  (λ (test?)
    (λ (new old l)
      (cond
        ((null? l)
         `())
        ((test? (car l) old)
         (cons new l))
        (else
         (cons (car l)
               ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (λ (test?)
    (λ (new old l)
      (cond
        ((null? l)
         `())
        ((test? (car l) old)
         (cons old
               (cons new (cdr l))))
        (else
         (cons (car l)
               ((insertR-f test?) new old (cdr l))))))))

(define insert-g
  (λ (test? g)
    (λ (new old l)
      (cond
        ((null? l)
         `())
        ((test? (car l) old)
         (cond
           ((eq? g `l)
            (cons new l))
           (else
            (cons old
                  (cons new (cdr l))))))
        (else
         (cons (car l)
               ((insertR-f test?) new old (cdr l))))))))

(define insert-eq?-l (insert-g eq? `l))

(insert-eq?-l `qoo `foo `(foo bar baz (foo bar) (bar)))
((insert-g eq? `r) `qoo `foo `(foo bar baz (foo bar) (bar)))

; Some `folder/reduce` thing practices are ommitted here.
; Author tell us to construct these functions (insertL, insertR, subset and rember)
; with some lambdas as parameters, making a `insert-g` high-order function template.

( define atom? 
  ( lambda ( exp )
    ( and
      ( not ( null? exp ) )
      ( not ( pair? exp ) ) ) ) )

( define operators `( + - * / ) )

( define in?
  ( lambda ( exp l )
    ( cond
      ( ( null? l )
        #f )
      ( ( eq? ( car l ) exp )
        #t )
      ( else
        ( in? exp ( cdr l ) ) ) ) ) )

( define operator?
  ( lambda ( exp )
    ( in? exp operators ) ) )

( define 1st-sub-aexp
  ( lambda ( aexp )
    ( car ( cdr aexp ) ) ) )

( define 2st-sub-aexp
  ( lambda ( aexp )
    ( car ( cdr ( cdr aexp ) ) ) ) )

( define operator
  ( lambda ( aexp )
    ( car aexp ) ) )

( define symbol-to-function
  ( lambda ( symbol )
    ( cond
      ( ( eq? symbol `+ ) + )
      ( ( eq? symbol `- ) - )
      ( ( eq? symbol `* ) * )
      ( ( eq? symbol `/ ) / ) ) ) )

( define value
  ( lambda ( exp )
    ( cond
      ( ( atom? exp )
        exp )
      ( ( operator? ( operator exp ) )
        ( ( symbol-to-function ( operator exp ) )
          ( value ( 1st-sub-aexp exp ) )
          ( value ( 2st-sub-aexp exp ) ) ) ) ) ) )

( value `( + 0 ( * 3 4 ) ) )

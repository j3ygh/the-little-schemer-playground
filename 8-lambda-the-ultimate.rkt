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
; Page. 148
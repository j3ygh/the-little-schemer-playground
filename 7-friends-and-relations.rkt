#lang racket

; Helpers
(define multirember
  (lambda (lat a)
    (cond
      ((null? lat)
       `())
      ((eq? (car lat) a)
       (multirember (cdr lat) a))
      (else
       (cons (car lat) (multirember (cdr lat) a))))))

(define in?
  (lambda (exp l)
    (cond
      ((null? l)
       #f)
      (else
       (or (eq? exp (car l))
           (in? exp (cdr l)))))))

(in? `foo `(foo bar baz))
(in? `foo `())

(define set?
  (lambda (l)
    (cond
      ((null? l)
       #t)
      ((in? (car l) (cdr l))
       #f)
      (else
       (set? (cdr l))))))

(set? `(foo bar baz))
(set? `(foo (bar) foo))

(define makeset
  (lambda (l)
    (cond
      ((null? l)
       `())
      ((in? (car l) (cdr l))
       (makeset (cdr l)))
      (else
       (cons (car l) (makeset (cdr l)))))))

(makeset `(foo bar bar baz baz foo 1 2 2 2))

(define makeset2
  (lambda (l)
    (cond
      ((null? l)
       `())
      (else
       (cons (car l)
             (makeset2
              (multirember (car l)
                           (cdr l))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1)
       #f)
      (else
       (and (in? (car set1) set2)
            (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1)
       #f)
      (else
       (or (in? (car set1) set2)
           (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) `())
      ((in? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1)
       set2)
      ((in? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? l-set)
       `())
      (else
       (intersect (car l-set)
                  (intersectall (cdr l-set)))))))

(define atom?
  (lambda (exp)
    (and (not (pair? exp)) (not (null? exp)))))

(define a-pair?
  (lambda (exp)
    (cond
      ((atom? exp) #f)
      ((null? exp) #f)
      ((null? (cdr exp)) #f)
      ((null? (cdr (cdr exp))) #f)
      (else #f))))

(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 `()))))

(define firsts
  (lambda (l)
    (cond
      ((null? l)
       `())
      (else
       (cons (first l)
             (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l)
       `())
      (else
       (cons (second l)
             (seconds (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

; Interesting question:
; Why we want to indent `cons` and `build` with different styles?

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel)
       `())
      (else
       (cons (revpair (car rel))
             (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

; (define cookies
;   (lambda ()
;     (bake
;      (quote (350 degrees))
;      (quote (12 minutes))
;      (mix
;       (quote (walnuts 1 cup))
;       (quote (chocolate-chips 16 ounces))
;       (mix
;        (mix
;         (quote (flou r 2 cups))
;         (quote (oatmeal 2 cups))
;         (quote (salt .5 teaspoon))
;         (quote (baking-powder 1 teaspoon))
;         (quote (baking-soda 1 teaspoon)))
;        (mix
;         (quote (eggs 2 large))
;         (quote (vanilla 1 teaspoon))
;         (cream
;           (quote (butter 1 cup))
;           (quote (sugar 2 cups)))))))))
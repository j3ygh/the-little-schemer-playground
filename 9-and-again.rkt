#lang racket

(define pick
  (lambda (index lat)
    (cond
      ((eq? index 1)
       (car lat))
      (else
       (pick (- index 1) (cdr lat))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else
       (eq? sorn a))
      )))

(keep-looking `foo 1 `(2 4 foo bar))
(keep-looking `foo 1 `(2 4 bar foo))

(define looking
  (lambda (a lat)
    (keep-looking a 1 lat)))

(looking `foo `(2 4 foo bar))
(looking `foo `(2 4 bar foo))

(define eternity
  (lambda (x)
    (eternity x)))

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

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

; 167
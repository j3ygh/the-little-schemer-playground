#lang racket
; Helpers
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t )
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; #t
(lat? '(bacon and eggs))

(define is-woman?
  (lambda (age gender)
    (cond
      ((< age 18) #f)
      ((eq? gender "male") #f)
      (#t #t)
      )))

#lang racket
; page 74

(define atom?
  (lambda (x)
    (and
      (not (pair? x))
      (not (null? x)))))

(atom? 14)

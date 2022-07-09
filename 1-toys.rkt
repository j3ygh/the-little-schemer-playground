#lang racket
; Helpers
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Practices
; #t
(atom? 'atom)
; #t
(atom? 'turkey)
; #t
(atom? '1492)
; #t
(atom? 'u)
; #t
(atom? '*abc$)
; #f
(atom? '(atom))
; 'a
(car '(a b c))
; (a b c)
(car '((a b c) x y z))
; '(peanut butter and jelly)
(cons 'peanut '(butter and jelly))
; '(apple)
(cons 'apple '())

; Notes
; 1. cons takes two arguments:
; the first one is any S-expression;
; the second one is any list.

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

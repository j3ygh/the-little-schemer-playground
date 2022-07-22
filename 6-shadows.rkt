#lang racket
; Try a new indentation style in this file.

; Helpers
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

( define atom? 
  ( lambda ( exp )
    ( and
      ( not ( null? exp ) )
      ( not ( pair? exp ) ) ) ) )

; Functions
( define numbered?
  ( lambda ( exp )
    ( cond
      ( atom?
        ( number? exp ) )
      ( else
        ( and
          ( operator? ( car exp ) )
          ( numbered? ( car ( cdr exp ) ) )
          ( numbered? ( car ( cdr ( cdr exp ) ) ) ) ) ) ) ) )

; Testings
( numbered? 42 )
( numbered? `foo )
( numbered? `() )
( numbered? `( + 1 2 ) )
( numbered? `( + 1 ( * 3 4 ) ) )

; Try a new indentation style in this file.
#lang racket

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

; Functions
( define numbered?
  ( lambda ( exp )
    ( cond
      ( ( null? exp )
        #f )
      ( ( list? exp )
        ( and
          ( operator? ( car exp ) )
          ( numbered? ( car ( cdr exp ) ) )
          ( numbered? ( car ( cdr ( cdr exp ) ) ) ) ) )
      ( ( number? exp )
        #t )
      ( else
        `foo ) ) ) )

; Testings
( numbered? 42 )
( numbered? `() )
( numbered? `( + 1 2 ) )
( numbered? `( + 1 ( * 3 4 ) ) )

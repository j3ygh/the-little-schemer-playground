#lang racket

; Try a new indentation style
( define operators `(+ - * /) )

( define in?
  ( lambda ( s l )
    ( cond
      ( ( null? l )
        #f )
      ( ( eq? ( car l ) s )
        #t )
      ( else
        ( in? s ( cdr l ) ) ) ) ) )

( define is-operator
  ( lambda ( s )
    ( in? s operators ) ) )

( define numbered?
  ( lambda ( aexp )
    ( cond
      ( ( null? aexp )
        #f )
      ( ( list? aexp )
        ( and
          ( is-operator aexp )
          ( numbered? ( car ( cdr aexp ) ) )
          ( numbered? ( car ( cdr ( cdr aexp ) ) ) ) ) )
      ( ( number? aexp )
        #t )
      ( else
        #f ) ) ) )

( numbered? 42 )
( numbered? `() )
( numbered? `( + 1 2 ) )
( numbered? `( + 1 ( * 3 4 ) ) )

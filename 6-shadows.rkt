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

( define value
  ( lambda ( exp )
    ( cond
      ( ( atom? exp )
        exp )
      ( ( eq? ( car exp ) `+ )
        ( +
          ( value ( car ( cdr exp ) ) )
          ( value ( car ( cdr ( cdr exp ) ) ) ) ) )
      ( ( eq? ( car exp ) `- )
        ( -
          ( value ( car ( cdr exp ) ) )
          ( value ( car ( cdr ( cdr exp ) ) ) ) ) )
      ( ( eq? ( car exp ) `* )
        ( *
          ( value ( car ( cdr exp ) ) )
          ( value ( car ( cdr ( cdr exp ) ) ) ) ) )
      ( ( eq? ( car exp ) `/ )
        ( /
          ( value ( car ( cdr exp ) ) )
          ( value ( car ( cdr ( cdr exp ) ) ) ) ) ) ) ) )

( value 42 )
( value `( + 1 2 ) )
( value `( + 1 ( * 3 4 ) ) )

; The Seventh Commandment
; Recur on the subparts that are of the same nature:
; • On the sublists of a list.
; • On the subexpressions of an arithmetic expression. 

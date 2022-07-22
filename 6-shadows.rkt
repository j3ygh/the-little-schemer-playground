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

( define 1st-sub-aexp
  ( lambda ( aexp )
    ( car ( cdr aexp ) ) ) )

( define 2st-sub-aexp
  ( lambda ( aexp )
    ( car ( cdr ( cdr aexp ) ) ) ) )

( define operator
  ( lambda ( aexp )
    ( car aexp ) ) )

( define value2
  ( lambda ( exp )
    ( cond
      ( ( atom? exp )
        exp )
      ( ( eq? ( operator exp ) `+ )
        ( +
          ( value2 ( 1st-sub-aexp exp ) )
          ( value2 ( 2st-sub-aexp exp ) ) ) )
      ( ( eq? ( operator exp ) `- )
        ( -
          ( value2 ( 1st-sub-aexp exp ) )
          ( value2 ( 2st-sub-aexp exp ) ) ) )
      ( ( eq? ( operator exp ) `* )
        ( *
          ( value2 ( 1st-sub-aexp exp ) )
          ( value2 ( 2st-sub-aexp exp ) ) ) )
      ( ( eq? ( operator exp ) `/ )
        ( /
          ( value2 ( 1st-sub-aexp exp ) )
          ( value2 ( 2st-sub-aexp exp ) ) ) ) ) ) )

( value2 42 )
( value2 `( + 1 2 ) )
( value2 `( + 1 ( * 3 4 ) ) )

; The Eighth Commandment
; Use help functions to abstract from representations.

( define zero?
  ( lambda ( exp )
    ( null? exp ) ) )

( zero? `() )

( define add-1
  ( lambda ( exp )
    ( cons `() exp ) ) )

( define sub-1
  ( lambda ( exp )
    ( cdr exp ) ) )

( add-1 `() )
( add-1 ( add-1 `() ) )
( add-1 ( add-1 ( add-1 `() ) ) )
( add-1 ( sub-1 ( add-1 `() ) ) )

( define plus
  ( lambda ( n m )
    ( cond
      ( ( zero? m )
        n )
      ( else
        ( plus ( add-1 n ) ( sub-1 m ) ) ) ) ) )

( plus `() `(() () () ()) )

; Recall lat ?
; Do you remember what the value of (lat ? ls)
; is where ls is (1 2 3)
; What is (1 2 3) with our new numbers?
; What is (lat? ls) where
; ls is ((()) (()(
; )) (()(
; )()))
; Is that bad?
; Shadows
; Easy:
; (define lat ?
; (lambda (l)
; (cond
; ((null? l) #t )
; (( atom? ( car l)) (lat? ( cdr l)))
; (else #f ))) )
; But why did you ask?
; #t , of course.
; ((()) (()()) (()()())) .
; It is very false.
; You must beware of shadows. 

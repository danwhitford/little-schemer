#lang racket
(require rackunit)

(define (atom? x)
  (and
   (not (pair? x))
   (not (null? x))))

(check-equal? (atom? '()) #f)

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (member? a lat)
  (cond
    ((null? lat) #f)
    (else (or
           (eq? a (car lat))
           (member? a (cdr lat))))))

(check-equal?
 (member? 'sardines '(italian sardines spaghetti parsley))
 #t)

(define (two-in-a-row? lat)
  (cond
    ((null? lat) #f)
    (else
     (is-first-b? (car lat) (cdr lat)))))

(define (is-first-b? a lat)
  (cond
    ((null? lat) #f)    
    (else
     (or
      (eq? a (car lat))
      (two-in-a-row? lat)))))

(check-equal?
 (two-in-a-row? '(italian sardines spaghetti parsley))
 #f)
(check-equal?
 (two-in-a-row? '(italian sardines sardines spaghetti parsley))
 #t)

(define (two-in-a-row-b? preceeding lat)
  (cond
    ((null? lat) #f)
    (else
     (or
      (eq? preceeding (car lat))
      (two-in-a-row-b? (car lat) (cdr lat))))))

(define (two-in-a-row-final? lat)
  (cond
    ((null? lat) #f)
    (else
     (two-in-a-row-b? (car lat) (cdr lat)))))

(check-equal?
 (two-in-a-row-final? '(italian sardines spaghetti parsley))
 #f)
(check-equal?
 (two-in-a-row-final? '(italian sardines sardines spaghetti parsley))
 #t)
(check-equal?
 (two-in-a-row-final? '(d e i i a g))
 #t)

(define (sum-of-prefixes lat)
  (cond
    ((null? lat) '())
    (else
     (sum-of-prefixes-b 0 lat))))

(define (sum-of-prefixes-b sumsofar tup)
  (cond
    ((null? tup) '())
    (else
     (cons
      (+ sumsofar (car tup))
      (sum-of-prefixes-b
       (+ sumsofar (car tup))
       (cdr tup))))))

(check-equal?
 (sum-of-prefixes '(2 1 9 17 0))
 '(2 3 12 29 29))
(check-equal?
 (sum-of-prefixes '(1 1 1 1 1))
 '(1 2 3 4 5))

(define (pick n lat)
  (cond
    ((eq? n 1) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

(check-equal? (pick 1 '(1 2 3 4 5)) 1)
(check-equal? (pick 3 '(1 2 3 4 5)) 3)
(check-equal? (pick 5 '(1 2 3 4 5)) 5)

(define (scramble tup)
  (cond
    ((null? tup) '())
    (else (scramble-b tup '()))))

(define (scramble-b tup reversed-prefix)
  (cond
    ((null? tup) '())
    (else
     (cons
      (pick (car tup) (cons (car tup) reversed-prefix))
      (scramble-b (cdr tup) (cons (car tup) reversed-prefix))))))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))
(check-equal? (scramble '(1 2 3 4 5 6 7 8 9)) '(1 1 1 1 1 1 1 1 1))
(check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10)) '(1 1 1 1 1 1 1 1 2 8 2))

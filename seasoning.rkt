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
     (two-in-a-row-b? (car lat) (cdr lat))
     (eq? preceeding (car lat))))))

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

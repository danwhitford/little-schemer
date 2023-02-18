#lang racket
(require rackunit)

(define (atom? x)
  (and
   (not (pair? x))
   (not (null? x))))

(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

(check-equal? (lat? '(Jack Sprat ate no chicken fat)) #t)
(check-equal? (lat? '((Jack) Sprat ate no chicken fat)) #f)
(check-equal? (lat? '(Jack (Sprat ate) no chicken fat)) #f)
(check-equal? (lat? '()) #t)

(define (member? a lat)
  (cond
    ((null? lat) #f)
    (else (or (eq? a (car lat))
              (member? a (cdr lat))))))

(check-equal? (member? 'tea '(coffee tea or milk)) #t)
(check-equal? (member? 'poached '(fried eggs and scrambled eggs)) #f)
(check-equal? (member? 'liver '(lox)) #f)
(check-equal? (member? 'liver '(and lox)) #f)
(check-equal? (member? 'liver '(bagels and lox)) #f)


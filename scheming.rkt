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
    ((eq? a (car lat)) #t)
    (else (member? a (cdr lat)))))

(check-equal? (member? 'tea '(coffee tea or milk)) #t)
(check-equal? (member? 'poached '(fried eggs and scrambled eggs)) #f)
(check-equal? (member? 'liver '(lox)) #f)
(check-equal? (member? 'liver '(and lox)) #f)
(check-equal? (member? 'liver '(bagels and lox)) #f)

(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a)
     (cdr lat))
    (else
     (cons (car lat) (rember a (cdr lat))))))

(check-equal? (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
(check-equal? (rember 'mint '(lamb chops and mint flavoured mint jelly)) '(lamb chops and flavoured mint jelly))
(check-equal? (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))
(check-equal? (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))

(define (firsts l)
  (cond
    ((null? l) '())
    (else (cons
           (car (car l))
           (firsts (cdr l))))))

(check-equal? (firsts
               '((apple peach pumpkin)
                 (plum pear cherry)
                 (grape raisin pea)
                 (bean carrot eggplant)))
              '(apple plum grape bean))
(check-equal? (firsts '((a b) (c d) (e f)))
              '(a c e))
(check-equal? (firsts '()) '())
(check-equal? (firsts
               '(((five plums) four)
                 (eleven green oranges)
                 ((no) more)))
              '((five plums) eleven (no)))

(define (insertR new old lat)
  (cond
    ((null? lat) '())
    ((equal? old (car lat))
     (cons old (cons new (cdr lat))))
    (else
     (cons (car lat) (insertR new old (cdr lat))))))

(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with fudge topping for dessert))
(check-equal? (insertR 'jalapeno 'and '(tacos tamales and salsa))
              '(tacos tamales and jalapeno salsa))
(check-equal? (insertR 'e 'd '(a b c d f g d h))
              '(a b c d e f g d h))

(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((equal? old (car lat))
     (cons new lat))
    (else
     (cons (car lat) (insertL new old (cdr lat))))))

(check-equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping fudge for dessert))
(check-equal? (insertL 'jalapeno 'and '(tacos tamales and salsa))
              '(tacos tamales jalapeno and salsa))
(check-equal? (insertL 'e 'd '(a b c d f g d h))
              '(a b c e d f g d h))

(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((equal? old (car lat))
     (cons new (cdr lat)))
    (else
     (cons (car lat) (subst new old (cdr lat))))))

(check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))

(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((or
      (equal? o1 (car lat))
      (equal? o2 (car lat)))
     (cons new (cdr lat)))
    (else
     (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(check-equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))

(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((equal? (car lat) a)
     (multirember a (cdr lat)))
    (else
     (cons (car lat) (multirember a (cdr lat))))))

(check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))
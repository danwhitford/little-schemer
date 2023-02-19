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
    ((eq? old (car lat))
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
    ((eq? old (car lat))
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
    ((eq? old (car lat))
     (cons new (cdr lat)))
    (else
     (cons (car lat) (subst new old (cdr lat))))))

(check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))

(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((or
      (eq? o1 (car lat))
      (eq? o2 (car lat)))
     (cons new (cdr lat)))
    (else
     (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(check-equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))

(define (multirember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a)
     (multirember a (cdr lat)))
    (else
     (cons (car lat) (multirember a (cdr lat))))))

(check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))

(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old)
     (cons old (cons new (multiinsertR new old (cdr lat)))))
    (else
     (cons (car lat) (multiinsertR new old (cdr lat))))))

(check-equal? (multiinsertR 'foo 'bar '(foo bar baz bar))
              '(foo bar foo baz bar foo))

(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old)
     (cons new (cons old (multiinsertL new old (cdr lat)))))
    (else
     (cons (car lat) (multiinsertL new old (cdr lat))))))

(check-equal? (multiinsertL 'foo 'bar '(foo bar baz bar))
              '(foo foo bar baz foo bar))
(check-equal? (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
              '(chips and fried fish or fried fish and fried))

(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old)
     (cons new (multisubst new old (cdr lat))))
    (else
     (cons (car lat) (multisubst new old (cdr lat))))))

(check-equal? (multisubst 'foo 'bar '(baz bar baz bar))
              '(baz foo baz foo))

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(check-equal? (add1 67) 68)
(check-equal? (sub1 5) 4)

(define (o+ n m)  
  (if (zero? m)
      n
      (add1 (o+ n (sub1 m)))))

(define (o- n m)
  (if (zero? m)
      n
      (sub1 (o- n (sub1 m)))))

(check-equal? (o+ 46 12) 58)
(check-equal? (o- 14 3) 11)
(check-equal? (o- 17 9) 8)

(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else (o+ (car tup) (addtup (cdr tup))))))

(check-equal? (addtup '(1 2 3)) 6)
(check-equal? (addtup '(1 2 3 4 5)) 15)

(define (X n m)  
  (cond
    ((zero? m) 0)
    (else (o+ n (X n (sub1 m))))))

(check-equal? (X 5 10) 50)
(check-equal? (X 3 4) 12)
(check-equal? (X 20 3) 60)
(check-equal? (X 12 3) 36)

(define (tup+ tup1 tup2)
  (cond    
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else
     (cons
      (o+ (car tup1) (car tup2))
      (tup+ (cdr tup1) (cdr tup2))))))

(check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
              '(11 11 11 11 11))
(check-equal? (tup+ '(2 3) '(4 6))              
              '(6 9))
(check-equal? (tup+ '(3 7) '(4 6))              
              '(7 13))
(check-equal? (tup+ '(3 7) '(4 6 8 1))              
              '(7 13 8 1))
(check-equal? (tup+ '(3 7 8 1) '(4 6))              
              '(7 13 8 1))

(define (o> n m)
  (cond
    ((zero? n) #f)
    ((zero? m) #t)    
    (else (o> (sub1 n) (sub1 m)))))

(check-equal? (o> 12 133) #f)
(check-equal? (o> 120 11) #t)
(check-equal? (o> 3 3) #f)

(define (o< n m)
  (cond
    ((zero? m) #f)
    ((zero? n) #t)
    (else (o< (sub1 n) (sub1 m)))))

(check-equal? (o< 4 6) #t)
(check-equal? (o< 8 3) #f)
(check-equal? (o< 6 6) #f)

(define (== n m)
  (cond
    ((o> n m) #f)
    ((o< n m) #f)
    (else #t)))

(check-equal? (== 4 6) #f)
(check-equal? (== 8 3) #f)
(check-equal? (== 6 6) #t)

(define (expo n m)
  (cond
    ((zero? m) 1)
    (else (X n (expo n (sub1 m))))))

(check-equal? (expo 1 1) 1)
(check-equal? (expo 2 3) 8)
(check-equal? (expo 5 3) 125)

(define (quoto n m)
  (cond
    ((o< n m) 0)
    (else (add1 (quoto (o- n m) m)))))

(check-equal? (quoto 15 4) 3)

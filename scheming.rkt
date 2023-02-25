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

(define (length lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat))))))

(check-equal? (length '(hotdogs with mustard cabbage and pickles)) 6)
(check-equal? (length '(ham and cheese on rye)) 5)

(define (pick n lat)
  (cond
    ((== n 1) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

(check-equal? (pick 4 '(lasagna spaghetti revioli macaroni meatball)) 'macaroni)

(define (rempick n lat)
  (cond
    ((== n 1) (cdr lat))
    (else
     (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(check-equal? (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard))

(define (no-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat))
     (no-nums (cdr lat)))
    (else
     (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond
    ((null? lat) '())
    ((not (number? (car lat)))
     (all-nums (cdr lat)))
    (else
     (cons (car lat) (all-nums (cdr lat))))))

(check-equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))

(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2))
     (== a1 a2))
    ((or (number? a1) (number? a2))
     #f)
    (else (eq? a1 a2))))

(define (occur a lat)
  (cond
    ((null? lat) 0)
    ((eqan? (car lat) a)
     (add1 (occur a (cdr lat))))
    (else
     (occur a (cdr lat)))))

(check-equal? (occur 'foo '(ham egg ham egg and chips)) 0)
(check-equal? (occur 'ham '(ham egg ham egg and chips)) 2)
(check-equal? (occur 'chips '(ham egg ham egg and chips)) 1)

(define (one? n)
  (eqan? n 1))

(define (rempick2 n lat)
  (cond
    ((one? n) (cdr lat))
    (else
     (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(check-equal? (rempick2 3 '(lemon meringue salty pie)) '(lemon meringue pie))

(define (rember* a lat)
  (cond
    ((null? lat) '())
    ((atom? (car lat))     
     (cond
       ((eq? (car lat) a)
        (rember* a (cdr lat)))
       (else
        (cons (car lat) (rember* a (cdr lat))))))
    (else
     (cons (rember* a (car lat)) (rember* a (cdr lat))))))

(check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))
(check-equal? (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
              '(((tomato)) ((bean)) (and ((flying)))))

(check-equal? (lat? '(((tomato sauce)) ((bean) sauce_ )and ((flying)) sauce))
              #f)

(define (insertR* new old l)  
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else
        (cons (car l) (insertR* new old (cdr l))))))
    (else
     (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))

(check-equal? (insertR*
               'roast
               'chuck
               '((how much (wood))
                 could
                 ((a (wood) chuck))
                 (((chuck)))
                 (if (a) ((wood chuck)))
                 could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast)))
                (if (a) ((wood chuck roast)))
                could chuck roast wood))

(define (occurs* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))
     (cond
       ((eq? (car l) a)
        (add1 (occurs* a (cdr l))))
       (else
        (occurs* a (cdr l)))))
    (else
     (o+ (occurs* a (car l)) (occurs* a (cdr l))))))

(check-equal? (occurs*
               'banana
               '((banana)
                 (split ((((banana ice)))
                         (cream (banana))
                         sherbet))
                 (banana)
                 (bread)
                 (banana brandy)))
              5)

(define (subst* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons new (subst* new old (cdr l))))
       (else
        (cons (car l) (subst* new old (cdr l))))))
    (else
     (cons (subst* new old (car l)) (subst* new old (cdr l))))))

(check-equal? (subst*
               'orange
               'banana
               '((banana)
                 (split ((((banana ice)))
                         (cream (banana))
                         sherbert))
                 (banana)
                 (bread)
                 (banana brandy)))
              '((orange)
                (split ((((orange ice)))
                        (cream (orange))
                        sherbert))
                (orange)
                (bread)
                (orange brandy)))

(define (insertL* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons new (cons old (insertL* new old (cdr l)))))
       (else
        (cons (car l) (insertL* new old (cdr l))))))
    (else
     (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))

(check-equal? (insertL*
               'pecker
               'chuck
               '((how much (wood))
                 could
                 ((a (wood) chuck))
                 (((chuck)))
                 (if (a) ((wood chuck)))
                 could chuck wood))
              '((how much (wood))
                could
                ((a (wood) pecker chuck))
                (((pecker chuck)))
                (if (a) ((wood pecker chuck)))
                could pecker chuck wood))

(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
    (else
     (or (member* a (car l)) (member* a (cdr l))))))

(check-equal? (member* 'chips '((potato) (chips ((with) fish) (chips)))) #t)

(define (leftmost l)
  (cond    
    ((atom? (car l))
     (car l))
    (else
     (leftmost (car l)))))

(check-equal? (leftmost '((potato) (chips ((with) fish) (chips)))) 'potato)

(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) ;; Both are empty
     #t)
    ((or (null? l1) (null? l2)) ;; Only one is empty
     #f)
    ((and (atom? (car l1)) (atom? (car l2))) ;; Both are atoms
     (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    ((or (atom? (car l1)) (atom? (car l2))) ;; One is an atom one is a list
     #f)    
    (else ;; Both are lists
     (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))

(check-equal? (eqlist? '(strawberry ice cream) '(strawberry ice cream)) #t)
(check-equal? (eqlist? '(strawberry ice cream) '(strawberry cream ice)) #f)
(check-equal? (eqlist? '((banana) split) '(banana (split))) #f)
(check-equal? (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) #f)
(check-equal? (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) #t)
(check-equal? (eqlist? '() '()) #t)
(check-equal? (eqlist? '() '(pancakes)) #f)

(define (equal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) ;; Both atoms
     (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) ;; One atom
     #f)
    (else ;; Both lists
     (eqlist? s1 s2))))

(check-equal? (equal? '(strawberry ice cream) '(strawberry ice cream)) #t)
(check-equal? (equal? '(strawberry ice cream) '(strawberry cream ice)) #f)
(check-equal? (equal? '((banana) split) '(banana (split))) #f)
(check-equal? (equal? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) #f)
(check-equal? (equal? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) #t)
(check-equal? (equal? '() '()) #t)
(check-equal? (equal? '() '(pancakes)) #f)
(check-equal? (equal? 'foo 'foo) #t)
(check-equal? (equal? 'foo 'bar) #f)

(define (eqlist2? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) ;; Both are empty
     #t)
    ((or (null? l1) (null? l2)) ;; Only one is empty
     #f) 
    (else ;; Both are lists
     (equal? l1 l2))))

(check-equal? (eqlist2? '(strawberry ice cream) '(strawberry ice cream)) #t)
(check-equal? (eqlist2? '(strawberry ice cream) '(strawberry cream ice)) #f)
(check-equal? (eqlist2? '((banana) split) '(banana (split))) #f)
(check-equal? (eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) #f)
(check-equal? (eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) #t)
(check-equal? (eqlist2? '() '()) #t)
(check-equal? (eqlist2? '() '(pancakes)) #f)

(define (numbered? aexp)
  (cond
    ((atom? aexp)
     (number? aexp))
    (else
     (and
      (numbered? (car aexp))
      (numbered? (car (cdr (cdr aexp))))))))

(check-equal? (numbered? 1) #t)
(check-equal? (numbered? 'pancakes) #f)
(check-equal? (numbered? '(2 + (4 * 5))) #t)
(check-equal? (numbered? '(2 f (4 g 5))) #t)
(check-equal? (numbered? '(2 * sausage)) #f)

(define (shadow->int ch)
  (cond
    ((null? ch) 0)
    (else
     (add1 (shadow->int (cdr ch))))))

(check-equal? (shadow->int '()) 0)
(check-equal? (shadow->int '( () )) 1)
(check-equal? (shadow->int '( ()() )) 2)
(check-equal? (shadow->int '( ()()()()() )) 5)

(define (value nexp)
  (cond
    ((atom? nexp)
     nexp)    
    ((eq? (operator nexp) '+)
     (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
    ((eq? (car nexp) '^)
     (expo (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))

(define (1st-sub-exp nexp)
  (car (cdr nexp)))

(define (2nd-sub-exp nexp)
  (car (cdr (cdr nexp))))

(define (operator nexp)
  (car nexp))

(check-equal? (value 13) 13)
(check-equal? (value '(+ 1 3)) 4)
(check-equal? (value '(+ 1 (^ 3 4))) 82)

(define (shadow-zero? n)
  (null? n))

(check-equal? (shadow-zero? '()) #t)
(check-equal? (shadow-zero? '(())) #f)

(define (shadow-add1 n)
  (cons '() n))

(check-equal? (shadow-add1 '()) '(()))
(check-equal? (shadow-add1 '(())) '(()()))

(define (shadow-sub1 n)
  (cdr n))

(check-equal? (shadow-sub1 '(())) '())
(check-equal? (shadow-sub1 '(()())) '(()))

(define (shadow-+ n m)
  (cond
    ((shadow-zero? m) n)
    (else
     (shadow-add1 (shadow-+ n (shadow-sub1 m))))))

(check-equal? (shadow-+ '() '()) '())
(check-equal? (shadow-+ '(()) '()) '(()))
(check-equal? (shadow-+ '(()) '(()())) '(()()()))
(check-equal? (shadow-+ '(()()) '(()())) '(()()()()))

(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))

(check-equal? (set? '(apple peaches apple plum)) #f)
(check-equal? (set? '(apple peaches pears plum)) #t)
(check-equal? (set? '(apple 3 pear 4 9 apple 3 4)) #f)

(define (makeset lat)
  (cond
    ((null? lat) '())
    (else
     (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))

(check-equal? (makeset '(apple peach pear peach
                               plum apple lemon peach))
              '(apple peach pear plum lemon))
(check-equal? (makeset '(apple 3 pear 4 9 apple 3 4))
              '(apple 3 pear 4 9))

(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    (else (and (member? (car set1) set2)
               (subset? (cdr set1) set2)))))

(check-equal? (subset? '(5 chicken wings) '(5 hamburgers
                                              2 pieces fried chicken and
                                              light duckling wings))
              #t)
(check-equal? (subset? '(4 pounds of horseradish)
                       '(four pounds chicken and
                              5 ounces horseradish))
              #f)
;; The Devil's Line

(define (eqset? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(check-equal? (eqset? '(6 large chickens with wings)
                      '(6 chickens with large wings))
              #t)

(define (intersect? set1 set2)
  (cond
    ((null? set1) #f)
    (else
     (or (member? (car set1) set2)
         (intersect? (cdr set1) set2)))))

(check-equal? (intersect? '(stewed tomatoes and macaroni)
                          '(macaroni and cheese))
              #t)

(define (intersect set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2)
     (cons (car set1) (intersect (cdr set1) set2)))
    (else
     (intersect (cdr set1) set2))))

(check-equal? (intersect '(stewed tomatoes and macaroni)
                         '(macaroni and cheese))
              '(and macaroni))

(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2)
     (union (cdr set1) set2))
    (else
     (cons (car set1) (union (cdr set1) set2)))))

(check-equal? (union '(stewed tomatoes and macaroni casserole)
                     '(macaroni and cheese))
              '(stewed tomatoes casserole macaroni and cheese))

(define (intersectall l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else     
     (intersect (car l-set) (intersectall (cdr l-set))))))

(check-equal? (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))
(check-equal? (intersectall '((6 pears and)
                              (3 peaches and 6 peppers)
                              (8 pears and 6 plums)
                              (and 6 prunes with some apples)))
              '(6 and))

(define (fun? rel)
  (set? (firsts rel)))

(check-equal? (fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
              #f)
(check-equal? (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
              #t)
(check-equal? (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
              #f)

(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f)))

(check-equal? (a-pair? '(pear pear)) #t)
(check-equal? (a-pair? '(3 7)) #t)
(check-equal? (a-pair? '(full (house))) #t)

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build a b)
  (cons a (cons b '())))

(define (third l)
  (car (cdr (cdr l))))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else
     (cons
      (revpair (car rel))
      (revrel (cdr rel))))))

(define (revpair pair)
  (build (second pair) (first pair)))

(check-equal? (revrel '((8 a) (pumpkin pie) (got sick)))
              '((a 8) (pie pumpkin) (sick got)))

(define (seconds lat)
  (cond
    ((null? lat) '())
    (else
     (cons (second (car lat)) (seconds (cdr lat))))))

(define (fullfun? fun)
  (set? (seconds fun)))

(define (rember-f test? a lat)
  (cond
    ((null? lat) '())
    ((test? (car lat) a) (cdr lat))
    (else (cons (car lat) (rember-f test? a (cdr lat))))))

(check-equal? (rember-f = 5 '(6 2 5 3))
              '(6 2 3))
(check-equal? (rember-f eq? 'jelly '(jelly beans are good))
              '(beans are good))
(check-equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
              '(lemonade and (cake)))

(define (eq?-c c)
  (lambda (x)
    (eq? x c)))

(define eq?-salad (eq?-c 'salad))
(check-equal? (eq?-salad 'salad) #t)
(check-equal? (eq?-salad 'chips) #f)

(define (rember-f2 test?)
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((test? (car lat) a) (cdr lat))
      (else
       (cons
        (car lat)
        ((rember-f2 test?) a (cdr lat)))))))

(define rember-eq? (rember-f2 eq?))
(check-equal? (rember-eq? 'tuna '(tuna salad is good)) '(salad is good))
(check-equal? ((rember-f2 eq?) 'tuna '(tuna salad is good)) '(salad is good))
(check-equal? ((rember-f2 eq?) 'tuna '(shrimp salad and tuna salad)) '(shrimp salad and salad))
(check-equal? ((rember-f2 eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)) '(equal? eqan? eqlist? eqpair?))

(define (insertL-f test?)
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((test? (car lat) old)
       (cons new lat))
      (else
       (cons (car lat) ((insertL-f test?) new old (cdr lat)))))))

(check-equal? ((insertL-f eq?) 'bacon 'eggs '(toast eggs coffee)) '(toast bacon eggs coffee))

(define (insertR-f test?)
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((test? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else
       (cons (car lat) ((insertR-f test?) new old (cdr lat)))))))

(check-equal? ((insertR-f eq?) 'bacon 'eggs '(toast eggs coffee)) '(toast eggs bacon coffee))

(define (insert-g test? insert-f)
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((test? (car lat) old)
       (insert-f new old (cdr lat)))
      (else
       (cons (car lat) ((insert-g test? insert-f) new old (cdr lat)))))))

(define (seqL new old lat)
  (cons new (cons old lat)))

(define (seqR new old lat)
  (cons old (cons new lat)))

(check-equal? ((insert-g eq? seqL)
               'bacon 'eggs '(toast eggs coffee))
              '(toast bacon eggs coffee))
(check-equal? ((insert-g eq? seqR)
               'bacon 'eggs '(toast eggs coffee))
              '(toast eggs bacon coffee))

(define (insert-g2 seq)
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (seq new old (cdr lat)))
      (else
       (cons (car lat) ((insert-g2 seq) new old (cdr lat)))))))

(check-equal? ((insert-g2 seqL)
               'bacon 'eggs '(toast eggs coffee))
              '(toast bacon eggs coffee))
(check-equal? ((insert-g2 seqR)
               'bacon 'eggs '(toast eggs coffee))
              '(toast eggs bacon coffee))
(check-equal? ((insert-g2 (lambda (new old l) l)) #f 'sausage '(pizza with sausage and bacon))
              '(pizza with and bacon))

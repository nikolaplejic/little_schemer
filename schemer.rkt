#lang racket

(require rackunit)

;; -----------------------------------------------------------------------------

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(check-eq? (atom? '()) #f)
(check-eq? (atom? '(1 2 3 4)) #f)
(check-eq? (atom? 'hotdog) #t) 

;; -----------------------------------------------------------------------------

(define lat?
  (λ (l)
    (cond ((null? l) #t)          
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(check-eq? (lat? '(1 2 3 4)) #t)
(check-eq? (lat? '(1 (2 3) 4)) #f)
(check-eq? (lat? '(hotdog)) #t)
(check-eq? (lat? '()) #t)

;; -----------------------------------------------------------------------------

(define member?
  (λ (a lat)
    (cond ((null? lat) #f)
          ((eq? a (car lat)) #t)
          (else (member? a (cdr lat))))))

(check-eq? (member? 'tea '(coffee tea or milk)) #t)
(check-eq? (member? 'poached '(fried eggs and scrambled eggs)) #f)
(check-eq? (member? 'meat '(mashed potatoes and meat gravy)) #t)

;; -----------------------------------------------------------------------------

;; "Rember" stands for "remove a member"

(define rember
  (λ (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))

(check-equal? (rember 'lettuce '(bacon lettuce and tomato)) '(bacon and tomato))
(check-equal? (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
(check-equal? (rember 'tomato '(bacon lettuce and tomato)) '(bacon lettuce and))

;; -----------------------------------------------------------------------------

(define firsts
  (λ (l)
    (cond ((null? l) '())
          (else (cons (car (car l)) (firsts (cdr l)))))))

(check-equal? (firsts '((apple peach pumpkin)
                        (plum pear cherry)
                        (grape raisin pea)
                        (bean carrot eggplant))) '(apple plum grape bean))
(check-equal? (firsts '((a b) (c d) (e f))) '(a c e))
(check-equal? (firsts '()) '())
(check-equal? (firsts '((five plums)
                        (four)
                        (eleven green oranges))) '(five four eleven))
(check-equal? (firsts '(((five plums) four)
                        (eleven green oranges)
                        ((no) more))) '((five plums) eleven (no)))

;; -----------------------------------------------------------------------------

(define insertR
  (λ (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))

(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with fudge topping for dessert))
(check-equal? (insertR 'jalapeno 'and '(tacos tamales and salsa))
              '(tacos tamales and jalapeno salsa))

;; -----------------------------------------------------------------------------

(define insertL
  (λ (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons new lat))
          (else (cons (car lat) (insertL new old (cdr lat)))))))

(check-equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping fudge for dessert))
(check-equal? (insertL 'jalapeno 'and '(tacos tamales and salsa))
              '(tacos tamales jalapeno and salsa))

;; -----------------------------------------------------------------------------

(define subst
  (λ (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons new (cdr lat)))
          (else (cons (car lat) (subst new old (cdr lat)))))))

(check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))

(define subst2
  (λ (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (eq? o1 (car lat))
               (eq? o2 (car lat))) (cons new (cdr lat)))
          (else (cons (car lat) (subst new o1 o2 (cdr lat)))))))

(check-equal? (subst2 'vanilla 'chocolate 'banana
                      '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))

;; -----------------------------------------------------------------------------

(define multirember
  (λ (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(check-equal? (multirember 'cup '(coffe cup tea cup and hick cup))
              '(coffe tea and hick))

;; -----------------------------------------------------------------------------

(define multiinsertR
  (λ (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat))
           (cons (car lat)
                 (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(check-equal? (multiinsertR 'z 'y '(x y x y)) '(x y z x y z))

;; -----------------------------------------------------------------------------

(define multisubst
  (λ (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))

(check-equal? (multisubst 'x 'y '(x y x y x y x y))
              '(x x x x x x x x))

;; -----------------------------------------------------------------------------
;; 4. NUMBERS GAMES
;; -----------------------------------------------------------------------------

(define add1
  (λ (n) (+ n 1)))

(check-equal? (add1 1) 2)
(check-equal? (add1 10) 11)

;; -----------------------------------------------------------------------------

(define sub1
  (λ (n) (- n 1)))

(check-equal? (sub1 1) 0)
(check-equal? (sub1 10) 9)

;; -----------------------------------------------------------------------------

(define o+
  (λ (x y)
    (cond ((zero? y) x)
          (else (o+ (add1 x) (sub1 y))))))

(check-equal? (o+ 1 1) 2)
(check-equal? (o+ 3 2) 5)

;; -----------------------------------------------------------------------------

(define o-
  (λ (x y)
    (cond ((zero? y) x)
          (else (o- (sub1 x) (sub1 y))))))

(check-equal? (o- 2 1) 1)
(check-equal? (o- 10 8) 2)

;; -----------------------------------------------------------------------------

(define addtup
  (λ (tup)
    (cond ((null? tup) 0)
          (else (o+ (car tup) (addtup (cdr tup)))))))

(check-equal? (addtup '(3 5 2 8)) 18)
(check-equal? (addtup '(15 6 7 12 3)) 43)

;; -----------------------------------------------------------------------------

(define o*
  (λ (x y)
    (cond ((zero? y) 0)
          (else (o+ x (o* x (sub1 y)))))))

(check-equal? (o* 5 3) 15)
(check-equal? (o* 13 4) 52)

;; -----------------------------------------------------------------------------

(define tup+
  (λ (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
(check-equal? (tup+ '(2 3) '(4 6)) '(6 9))

;; -----------------------------------------------------------------------------

;; I kept using the 'o' prefix notation for all operators for consistency's
;; sake, as well as to avoid clashing with Racket's core operators.

(define o>
  (λ (x y)
    (cond 
      ((zero? x) #f)
      ((zero? y) #t)
      (else (o> (sub1 x) (sub1 y))))))

(check-equal? (o> 12 133) #f)
(check-equal? (o> 120 11) #t)
(check-equal? (o> 3 3) #f)

;; -----------------------------------------------------------------------------

(define o<
  (λ (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (o< (sub1 x) (sub1 y))))))

(check-equal? (o< 4 6) #t)
(check-equal? (o< 8 3) #f)
(check-equal? (o< 6 6) #f)

;; -----------------------------------------------------------------------------

(define o=
  (λ (x y)
    (cond
      ((o> x y) #f)
      ((o< x y) #f)
      (else #t))))

(check-equal? (o= 4 6) #f)
(check-equal? (o= 8 3) #f)
(check-equal? (o= 6 6) #t)

;; -----------------------------------------------------------------------------

(define o^
  (λ (x y)
    (cond
      ((zero? y) 1)
      (else (o* x (o^ x (sub1 y)))))))

(check-equal? (o^ 1 1) 1)
(check-equal? (o^ 2 3) 8)
(check-equal? (o^ 5 3) 125)

;; -----------------------------------------------------------------------------

(define o÷
  (λ (x y)
    (cond
      ((< x y) 0)
      (else (add1 (o÷ (o- x y) y))))))

(check-equal? (o÷ 15 4) 3)

;; -----------------------------------------------------------------------------

;; Again, the suffix "1" here is to avoid clashing with Racket's core lib. Not
;; sure if it's necessary, but seems like good practice.
;; In other news, I'm hungry.

(define length1
  (λ (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(check-equal? (length1 '(hotdogs with mustard sauerkraut and pickles)) 6)
(check-equal? (length1 '(ham and cheese on rye)) 5)

;; -----------------------------------------------------------------------------

(define pick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(check-equal? (pick 4 '(lasagna spaghetti ravioli macaroni meatball))
              'macaroni)

;; -----------------------------------------------------------------------------

(define rempick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(check-equal? (rempick 3 '(hotdogs with hot mustard))
              '(hotdogs with mustard))

;; (rempick 3 '(hotdogs with hot mustard))
;; => (cons 'hotdogs (rempick 2 '(with hot mustard)))
;; => (cons 'hotdogs (cons 'with (rempick 1 '(hot mustard))))
;; => (cons 'hotdogs (cons 'with '(mustard)))

;; -----------------------------------------------------------------------------

(define no-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(check-equal? (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))

;; (no-nums '(5 pears 6 prunes 9 dates))
;; => (no-nums '(pears 6 prunes 9 dates))
;; => (cons 'pears (no-nums '(6 prunes 9 dates)))
;; => (cons 'pears (no-nums '(prunes 9 dates)))
;; => (cons 'pears (cons 'prunes (no-nums '(9 dates))))
;; => (cons 'pears (cons 'prunes (no-nums '(dates))))
;; => (cons 'pears (cons 'prunes (cons 'dates '())))

;; -----------------------------------------------------------------------------

(define all-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(check-equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))

;; -----------------------------------------------------------------------------

(define eqan?
  (λ (x y)
    (cond
      ((and (number? x) (number? y)) (o= x y))
      ((or (number? x) (number? y)) #f)
      (else (eq? x y)))))

;; -----------------------------------------------------------------------------

(define occur
  (λ (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(check-equal? (occur 1 '(1 2 3 1)) 2)
(check-equal? (occur 'a '(a b b a)) 2)

;; -----------------------------------------------------------------------------

(define one?
  (lambda (n) (= n 1)))

;; -----------------------------------------------------------------------------

(define rempick1
  (λ (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick1 (sub1 n) (cdr lat)))))))

(check-equal? (rempick1 3 '(hotdogs with hot mustard))
              '(hotdogs with mustard))
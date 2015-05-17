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

(define sub1
  (λ (n) (- n 1)))

(define o+
  (λ (x y)
    (cond ((zero? y) x)
          (else (o+ (add1 x) (sub1 y))))))

(define o-
  (λ (x y)
    (cond ((zero? y) x)
          (else (o- (sub1 x) (sub1 y))))))
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

(check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))

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

;; -----------------------------------------------------------------------------
;; 5. *OH MY GAWD*: IT'S FULL OF STARS
;; -----------------------------------------------------------------------------

(define rember*
  (λ (a l)
    (cond ((null? l) l)
          ((not (atom? (car l))) (cons (rember* a (car l))
                                       (rember* a (cdr l))))
          ((eq? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l)))))))

;; holy guacamole that's a lot of parens
(check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))
(check-equal? (rember* 'sauce '(((tomato sauce)) ((bean) sauce)
                                                 (and ((flying)) sauce)))
              '(((tomato)) ((bean)) (and ((flying)))))

;; -----------------------------------------------------------------------------

(define insertR*
  (λ (new old l)
    (cond ((null? l) '())
          ((not (atom? (car l))) (cons (insertR* new old (car l))
                                       (insertR* new old (cdr l))))
          ((eq? old (car l)) (cons (car l)
                                   (cons new
                                         (insertR* old new (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l)))))))

(check-equal? (insertR* 'roast 'chuck
                        '((how much (wood)) could ((a (wood) chuck))
                                            (((chuck)))
                                            (if (a) ((wood chuck)))
                                            could chuck wood))
              '((how much (wood)) could ((a (wood) chuck roast))
                                            (((chuck roast)))
                                            (if (a) ((wood chuck roast)))
                                            could chuck roast wood))

;; -----------------------------------------------------------------------------

(define occur*
  (λ (a l)
    (cond
      ((null? l) 0)
      ((not (atom? (car l))) (o+ (occur* a (car l))
                                 (occur* a (cdr l))))
      ((eqan? a (car l)) (add1 (occur* a (cdr l))))
      (else (occur* a (cdr l))))))

(check-equal? (occur* 'banana '((banana)
                                (split ((((banana ice)))
                                        (cream (banana))
                                        sherbet))
                                (banana)
                                (bread)
                                (banana brandy)))
              5)

;; -----------------------------------------------------------------------------

(define subst*
  (λ (new old l)
    (cond ((null? l) '())
          ((not (atom? (car l))) (cons (subst* new old (car l))
                                       (subst* new old (cdr l))))
          ((eq? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l)))))))

(check-equal? (subst* 'orange 'banana
                      '((banana)
                        (split ((((banana ice)))
                                (cream (banana))
                                sherbet))
                        (banana)
                        (bread)
                        (banana brandy)))
              '((orange)
                (split ((((orange ice)))
                        (cream (orange))
                        sherbet))
                (orange)
                (bread)
                (orange brandy)))

;; -----------------------------------------------------------------------------

(define insertL*
  (λ (new old l)
    (cond ((null? l) '())
          ((not (atom? (car l))) (cons (insertL* new old (car l))
                                       (insertL* new old (cdr l))))
          ((eq? old (car l)) (cons new
                                   (cons (car l)
                                         (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l)))))))

(check-equal? (insertL* 'pecker 'chuck
                        '((how much (wood)) could ((a (wood) chuck))
                                            (((chuck)))
                                            (if (a) ((wood chuck)))
                                            could chuck wood))
              '((how much (wood)) could ((a (wood) pecker chuck))
                                  (((pecker chuck)))
                                  (if (a) ((wood pecker chuck)))
                                  could pecker chuck wood))

;; -----------------------------------------------------------------------------

(define member*
  (λ (a l)
    (cond ((null? l) #f)
          ((not (atom? (car l))) (or (member* a (car l))
                                     (member* a (cdr l))))
          ((eq? a (car l)) #t)
          (else (member* a (cdr l))))))

(check-equal? (member* 'chips '((potato) 
                                (chips ((with) fish)
                                       (chips))))
              #t)

;; -----------------------------------------------------------------------------

(define leftmost
  (λ (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))

(check-equal? (leftmost '((potato) 
                          (chips ((with) fish)
                                 (chips))))
              'potato)

;; -----------------------------------------------------------------------------

(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (atom? l1) (atom? l2)) (eqan? l1 l2))
      ((and (atom? l1) (not (atom? l2))) #f)
      ((and (not (atom? l1)) (atom? l2)) #f)
      ((and (atom? (car l1))
            (atom? (car l2))
            (eqan? (car l1) (car l2))) (eqlist? (cdr l1) (cdr l2)))
      (else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

;; this version, obviously, asks less questions than in the book.
;; however, it passes all of the tests within the book, and - to
;; me - seems consistent. YMMV. logic is hard, yo.

(check-equal? (eqlist? '(a b c) '(a b c)) #t)
(check-equal? (eqlist? '(strawberry ice cream)
                       '(strawberry cream ice)) #f)
(check-equal? (eqlist? '(banana ((split)))
                       '((banana) (split))) #f)
(check-equal? (eqlist? '(beef ((sausage)) (and (soda)))
                       '(beef ((salami)) (and (soda)))) #f)
(check-equal? (eqlist? '(beef ((sausage)) (and (soda)))
                       '(beef ((sausage)) (and (soda)))) #t)

;; -----------------------------------------------------------------------------

(define equal?
  (λ (s1 s2)
    (cond
      ;; the book uses a bit different conditions and
      ;; recurses on eqlist? instead of equal?
      ;; not sure why, perhaps I'm missing something.
      ;; @TODO
      ((and (null? s1) (null? s2)) #t)
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((null? s1) #f)
      (else (and (equal? (car s1) (car s2))
                 (equal? (cdr s1) (cdr s2)))))))

;; -----------------------------------------------------------------------------
;; 6. SHADOWS
;; -----------------------------------------------------------------------------

;; I am a bit bewildered by this excercise. It may become clearer afterwards.
;; However, currently, it seems we're treating all of the operators here as
;; infix operators instead of prefix operators (as they are in Scheme). Why?

;; I have also used our previously defined o^ operator instead of the "up
;; arrow" operator used in the book.

(define unsimplified-numbered?
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+) (and (unsimplified-numbered?
                                       (car aexp))
                                      (unsimplified-numbered?
                                       (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '-) (and (unsimplified-numbered?
                                       (car aexp))
                                      (unsimplified-numbered?
                                       (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'o^) (and (unsimplified-numbered?
                                        (car aexp))
                                       (unsimplified-numbered?
                                        (car (cdr (cdr aexp))))))
      (else #f))))

(check-equal? (unsimplified-numbered? '(2 + 3)) #t)
(check-equal? (unsimplified-numbered? '(2 + (2 + 3))) #t)
(check-equal? (unsimplified-numbered? '(2 + (2 + a))) #f)
(check-equal? (unsimplified-numbered? '(2 + (2 o^ 5))) #t)
(check-equal? (unsimplified-numbered? '(2 + (2 o^ a))) #f)

(define numbered?
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(check-equal? (numbered? '(2 + 3)) #t)
(check-equal? (numbered? '(2 + (2 + 3))) #t)
(check-equal? (numbered? '(2 + (2 + a))) #f)
(check-equal? (numbered? '(2 + (2 o^ 5))) #t)
(check-equal? (numbered? '(2 + (2 o^ a))) #f)

;; -----------------------------------------------------------------------------

;; Oh I see -- we'll basically write our own interpreter within the 'value'
;; function. Hah, take that, macros!

;; I have a distinct feeling we'll reimplement Scheme by the end of this book.

(define value
  (λ (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (car (cdr exp)) '+) (+ (value (car exp))
                                   (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '-) (- (value (car exp))
                                   (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) 'o^) (o^ (value (car exp))
                                     (value (car (cdr (cdr exp)))))))))

(check-equal? (value '13) 13)
(check-equal? (value '(1 + 3)) 4)
(check-equal? (value '(1 + (3 o^ 4))) 82)

;; -----------------------------------------------------------------------------

;; Same evaluator, but for prefix notation.

(define value1
  (λ (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (car exp) '+) (+ (value1 (car (cdr exp)))
                             (value1 (car (cdr (cdr exp))))))
      ((eq? (car exp) '-) (- (value1 (car (cdr exp)))
                             (value1 (car (cdr (cdr exp))))))
      ((eq? (car exp) 'o^) (o^ (value1 (car (cdr exp)))
                               (value1 (car (cdr (cdr exp)))))))))

(check-equal? (value1 '13) 13)
(check-equal? (value1 '(+ 1 3)) 4)
(check-equal? (value1 '(+ 1 (o^ 3 4))) 82)

;; -----------------------------------------------------------------------------

;; Same prefix evaluator, but with helpers.

(define 1st-sub-exp
  (λ (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (λ (aexp)
    (car aexp)))

(define value2
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (+ (value2 (1st-sub-exp nexp))
                                   (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '-) (- (value2 (1st-sub-exp nexp))
                                   (value2 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'o^) (o^ (value2 (1st-sub-exp nexp))
                                     (value2 (2nd-sub-exp nexp)))))))

(check-equal? (value2 '13) 13)
(check-equal? (value2 '(+ 1 3)) 4)
(check-equal? (value2 '(+ 1 (o^ 3 4))) 82)

;; -----------------------------------------------------------------------------

;; Same infix evaluator, but with helpers.

;; By now, the naming is getting ridiculous.

(define 1st-sub-exp1
  (λ (aexp)
    (car aexp)))

(define 2nd-sub-exp1
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define operator1
  (λ (aexp)
    (car (cdr aexp))))

(define value3
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator1 nexp) '+) (+ (value3 (1st-sub-exp1 nexp))
                                    (value3 (2nd-sub-exp1 nexp))))
      ((eq? (operator1 nexp) '-) (- (value3 (1st-sub-exp1 nexp))
                                    (value3 (2nd-sub-exp1 nexp))))
      ((eq? (operator1 nexp) 'o^) (o^ (value3 (1st-sub-exp1 nexp))
                                      (value3 (2nd-sub-exp1 nexp)))))))

(check-equal? (value3 '13) 13)
(check-equal? (value3 '(1 + 3)) 4)
(check-equal? (value3 '(1 + (3 o^ 4))) 82)

;; -----------------------------------------------------------------------------

(define sero?
  (λ (n)
    (null? n)))

(define edd1
  (λ (n)
    (cons '() n)))

(define zub1
  (λ (n)
    (cdr n)))

(define o+1
  (λ (x y)
    (cond
      ((sero? y) x)
      (else (o+1 (edd1 x) (zub1 y))))))

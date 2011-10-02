"The Little Schemer"
"Joshua Olson"

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (1+ x)))

(define sub1
  (lambda (x)
    (-1+ x)))

(define lat?
  (lambda (l)
    (cond 
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))	    
     (else (cons (car lat) 
		 (rember a 
			 (cdr lat)))))))

;This version of firsts can handle lists that are mixed
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     ((list? (car l)) (cons (car (car l)) 
			    (firsts (cdr l))))
     (else (firsts (cdr l))))))

;A generalized version of firsts 
;;Enclosing scope must define the following
;;Takes a description of the nth element
;;Takes a test for the nth element
(define nths
  (lambda (l test nth)
    (cond
     ((null? l) '())
     ((test (car l)) (cons (nth (car l)) 
			    (nths (cdr l) test nth)))
     (else (nths (cdr l) test nth)))))

(define seconds
  (lambda (l)
    ;Test for a second atom in the list
    (define test
      (lambda (list)
	(and
	 (not (null? list))
	 (list? (cdr list))
	 (atom? (cadr list)))))
    ;Define the nth element
    (define nth
      (lambda (list)
	(cadr list)))
    (nths l test nth)))
	 
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old 
				(cons new 
				      (cdr lat))))
     (else (cons (car lat) 
		 (insertR new 
			  old 
			  (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat) 
		 (insertL new 
			  old 
			  (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) 
		 (subst new 
			old 
			(cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1) 
	  (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else (cons (car lat) 
		 (subst2 new 
			 o1
			 o2
			 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old 
				(cons new 
				      (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new 
				(cons old 
				      (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new
				(multisubst new old (cdr lat))))
     (else (cons (car lat)
		 ( multisubst new old (cdr lat)))))))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (x n (sub1 m)))))))

(define tup+ 
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2))
		 (tup+ (cdr tup1) (cdr tup2)))))))

(define gt
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lt (sub1 n) (sub1 m))))))

(define eq
  (lambda (n m)
    (cond
     ((and (zero? m) (zero? n)) #t)
     ((or (zero? m) (zero? n)) #f)
     (else (eq (sub1 n) (sub1 m))))))

(define eq
  (lambda (n m)
    (cond
     ((lt n m) #f)
     ((gt n m) #f)
     (else #t))))

(define o^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (o^ n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) 
		 (rempick (sub1 n) 
			  (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
		 (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat)
				(all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= 1 n)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n)
			  (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (rember* a (cdr l)))
       (else (cons (car l)
		   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
		 (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons old 
				(cons new 
				      (insertR* new old (cdr l)))))
       (else (cons (car l)
		   (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l))
		 (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (1+ (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l))
	      (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons new 
				(subst* new old (cdr l))))
       (else (cons (car l)
		   (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l))
		 (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons new
				(cons old 
				      (insertL* new old (cdr l)))))
       (else (cons (car l)
		   (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l))
		 (insertL* new old (cdr l)))))))

(define member* 
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or 
       (eq? a (car l))
       (member* a (cdr l))))
     (else (or (member* a (car l))
	       (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1))
	  (atom? (car l2))) #f)
     (else (and (eqlist? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2)))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
		 (rember s (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((member? (car (cdr aexp)) '(+ * ^)) (and (numbered? (car aexp))
					       (numbered? (car (cdr (cdr aexp))))))
     (else #f))))

(define operator
  (lambda (aexp)
    (cadr aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operation
  (lambda (op operations)
    (cond
     ((null? operations) #f)
     ((eq? op (caar operations)) (cadar operations))
     (else (operation op (cdr operations))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
	   (o+ (value (1st-sub-exp nexp))
	       (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '*)
	   (o* (value (1st-sub-exp nexp))
	       (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '^)
	   (o^ (value (1st-sub-exp nexp))
	       (value (2nd-sub-exp nexp)))))))

; Page break
(restart 1)

(value '3)

(operation '+ '((+ o+) (* o*) (^ o^)))

(value '(1 + (2 ^ 5)))

(numbered? '(1 + (2 * 5)))

(numbered? '(1 + 3))

(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))

(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))

(eqlist? '(banana (split)) '((banana) (split)))

(eqlist? '(strawberry ice cream) '(strawberry cream ice))

(leftmost (quote ()))

(leftmost '(((hot) (tuna (and))) cheese))

(leftmost '(((() four)) 17 (seventeen)))

(member* 'chips '((potato) (chips ((with) fish) (chips))))

(insertL* 'pecker 'chuck '((how much (wood))
			  could
			  ((a (wood) chuck))
			  (((chuck)))
			  (if (a) ((wood chuck)))
			  could chuck wood))

(subst* 'orange 'banana '((banana)
		  (split ((((banana ice)))
			  (cream (banana))
			  sherbet))
		  (banana)
		  (bread)
		  (banana brandy)))

(occur* 'banana '((banana)
		  (split ((((banana ice)))
			  (cream (banana))
			  sherbet))
		  (banana)
		  (bread)
		  (banana brandy)))

(insertR* 'roast 'chuck '((how much (wood))
			  could
			  ((a (wood) chuck))
			  (((chuck)))
			  (if (a) ((wood chuck)))
			  could chuck wood))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(one? 1)

(one? 0)

(one? 13)

(occur '5 '(5 pears pears 6 prunes 9 9 dates))

(all-nums '(5 pears 6 prunes 9 dates))

(no-nums '(5 pears 6 prunes 9 dates))

(number? 76)

(number? 'tomato)

(rempick 3 '(hotdogs with hot mustard))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(length '(hotdogs with mustard sauerkraut and pickles))

(o/ 12 5)

(o/ 5 6)

(o^ 2 2)

(eq? 'test 'test)

(eq? 5 5)

(eq 6 6)

(eq 12 133)

(eq 5 4)

(lt 12 133)

(lt 6 6 )

(lt 5 4)

(gt 12 133)

(gt 6 6)

(gt 5 4)

(tup+ '(3 7) '(4 6 8 1))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))

(x 2 30)

(addtup '(15 6 7 12 3))

(o- 14 3)

(o+ 46 12)

(sub1 12)

(zero? 12)

(zero? 0)

(multisubst 'jelly 'd '(a b c d f g d h))

(multiinsertL 'peanut 'd '(a b c d f g d h))

(multiinsertR 'cat 'd '(a b c d f g d h))

(multirember 'cup '(coffee cup tea cup and hick cup))

(subst2 'penguin 'd 'b '(a b c d f g d h))

(subst 'pen 'd '(a b c d f g d h))

(insertL 'cat 'd '(a b c d f g d h))

(insertR 'e 'd '(a b c d f g d h))

(seconds '((a b) (c d) (e f)))

(atom? (car (cdr (car '((a b))))))

(pair? '(a b c))

(firsts '((a b) (c d) (e f)))

(firsts '((a b) c d (e f)))

(lat? '(bacon and eggs))

(lat? '(bacon (and eggs)))

(list? (car '((tomato))))

(member? 'meat '(mashed potatoes and meat gravy))

(member? 'meat '(mashed potatoes and (meat gravy)))

(member? 'liver '(mashed potatoes and meat gravy))

(rember 'jelly '(bacon lettuce and tomato))

(rember 'and '(bacon lettuce and tomato))

(add1 3)
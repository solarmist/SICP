"The Little Schemer"
"Joshua Olson"

"Chapter 1"

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (1+ x)))

(define sub1
  (lambda (x)
    (-1+ x)))

"Chapter 2"

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

"Chapter 3"

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))	    
     (else (cons (car lat) 
		 (rember a 
			 (cdr lat)))))))

; This version of firsts can handle lists that are mixed
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     ((list? (car l)) (cons (car (car l)) 
			    (firsts (cdr l))))
     (else (firsts (cdr l))))))

; A generalized version of firsts 
; Enclosing scope must define the following
; Takes a description of the nth element
; Takes a test for the nth element
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

"Chapter 4"

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

(define o* x)

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

"Chapter 5"

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

"Chapter 6"

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

"Chapter 7"

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat) (makeset (multirember (car lat) 
						 (cdr lat))))))))

;Assume that they are sets already
(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2) 
		(subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
	       (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) 
      (cons (car set1) 
	    (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1) 
		 (union (cdr set1) 
			set2))))))

(define difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (difference (cdr set1) set2))
     (else (cons (car set1)
		 (difference (cdr set1)
			     set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
		      (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
     ((or (atom? x) (null? x) (null? (cdr x))) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
		 (revrel (cdr rel)))))))

(define fullfun?
  (lambda (rel)
    (and (fun? rel) (fun? (revrel rel)))))

"Chapter 8"

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (x)
    (lambda (a)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a l)))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons new (cons old (cdr l))))
       (else (cons (car l)
		   ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons old (cons new (cdr l))))
       (else (cons (car l)
		   ((insertR-f test?) new old (cdr l))))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old) 
	(seq new old (cdr l)))
       (else (cons (car l)
		   ((insert-g seq) 
		     new 
		     old 
		     (cdr l))))))))

(define insertL 
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define subst 
  (insert-g (lambda (new old l)
	      (cons new l))))

(define rember
  (lambda (a l)
    ((insert-g (lambda (new old l) 
		 l))
     #f a l)))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) o+)
     ((eq? x '*) o*)
     (else o^))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
	    (value (1st-sub-exp nexp))
	    (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
	((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
		   ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat)
		 (multiremberT test?
			       (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      ; Collects items that match in the second lst
      (multirember&co a 
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     ; Collects everything else in the first lst
     (else (multirember&co a 
			   (cdr lat)
			   (lambda (newlat seen)
			     (col (cons (car lat) newlat)
				  seen)))))))
(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
	 (cons 'tuna seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat)
	      seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
	    (cons oldL
		 (multiinsertLR new oldL oldR
				(cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
	    (cons new
		  (multiinsertLR new oldL oldR
				 (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertLR new oldL oldR
			   (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) 
      (col (quote ()) 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons new 
				     (cons oldL 
					   newlat)) 
			       (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons oldR 
				     (cons new 
					   newlat)) 
			       L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR (cdr lat)
		     (lambda (newlat L R)
		       (col (cons (car lat) 
				  newlat) 
			    L R)))))))

(define lat-friend
  (lambda (lat L R)
    lat))

(define L-friend
  (lambda (lat L R)
    L))

(define even?
  (lambda (n)
    (= (o* (o/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l)) (cons (car l) 
			      (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
		 (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l)) (evens-only*&co (cdr l)
				       (lambda (newl p s)
					 (col (cons (car l) newl)
					      (o* (car l) p) s))))
       (else (evens-only*&co (cdr l)
			     (lambda (newl p s)
			       (col newl
				    p (o+ (car l) s)))))))
     (else (evens-only*&co (car l)
			   (lambda (al ap as)
			     (evens-only*&co (cdr l)
					     (lambda (dl dp ds)
					       (col (cons al dl)
						    (o* ap dp)
						    (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

; Page break
(restart 1)

"Chapter 8"

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) 
		the-last-friend)

(evens-only* '((9 1 2 8) 3 22 ((9 9) 7 6) 2))

(even? 2)

(multiinsertLR&co 'salty 'fish 'chips 
		  '(chips and fish or fish and chips) 
		  L-friend)

(multiinsertLR&co 'salty 'fish 'chips 
		  '(chips and fish or fish and chips) 
		  lat-friend)

(multiinsertLR 'fat 'cat 'cat '(one old cat and young dog))

(multiinsertLR 'fat 'cat 'dog '(one old cat and young dog))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)

(latest-friend '(and) '(tuna))

(new-friend '() '())

(multirember&co 'tuna '(tuna) a-friend)

(multirember&co 'tuna '() a-friend)

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(value '(5 + 3))

(rember 'cat '(pasta cat car))

(subst 'dog 'cat '(pasta cat car))

(insertR 'dog 'cat '(pasta cat car))

((eq?-c 'salad) 'tuna)

(eq?-salad 'salad)

(eq?-c 'salad)

((rember-f eq?) 'jelly '(jelly beans are good))

"Chapter 7"

(fullfun? '((grape raisin)
	    (plum prune)
	    (stewed grape)))

(fullfun? '((1 2) (2 3) (3 2)))

(fullfun? '((1 2) (2 3) (3 4)))

(revrel '((1 2) (2 4) (5 6)))

(revpair '(1 2))

(fun? '((4 3) (4 2) (1 3)))

(fun? '((1 2) (2 5) (5 6)))

(build 'full '(house))

(first '(full (house)))

(second '(full (house)))

(a-pair? '((this cat)))

(a-pair? '(full cat))

(a-pair? '(full (house)))

(intersectall '((a b c)
		(c a d e)
		(e f g h a b)))

(intersectall '((6 pears and)
		(3 peaches and 6 peppers)
		(8 pears and 6 plums)
		(and 6 prunes with some apples)))

(difference '(macaroni and cheese)
	    '(and))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

(intersect '(stewed tomatoes and macaroni)
	   '(macaroni and cheese))

(intersect? '(stewed tomatoes and macaroni)
	    '(6 chickens with large wings))

(intersect? '(stewed tomatoes and macaroni)
	    '(macaroni and cheese))

(eqset? '(6 large chickens with wings)
	'(6 chickens with large wings))

(subset? '(5 chicken wings) '(5 hambergers
				2 pieces fried chicken and
				light duckling wings))

(subset? '(4 pounds of horseradish) '(four pounds of chicken and
					   5 ounces of horseradish))

(makeset '(apple peach pear peach plum apple lemon peach))

(makeset '(apple 3 pear 4 9 3 7 plum))

(set? '(apple 3 pear 4 9 7 plum))

(set? '(apple 3 pear 4 9 3 7 plum))

(set? '(apples peaches pears plums))

(set? '(apple peaches apple plum))

"Chapter 6"

(value '3)

(operation '+ '((+ o+) (* o*) (^ o^)))

(value '(1 + (2 ^ 5)))

(numbered? '(1 + (2 * 5)))

(numbered? '(1 + 3))

"Chapter 5"

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

"Chapter 4"

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

"Chapters 1, 2 and 3"

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
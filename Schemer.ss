"The Little Schemer"
"Joshua Olson"

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

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
  (lambda (x y)
    (cond
     ((zero? y) x)
     (else (add1 (o+ x (sub1 y)))))))

(define o-
  (lambda (x y)
    (cond
     ((zero? y) x)
     (else (sub1 (o- x (sub1 y)))))))

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

; Page break

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
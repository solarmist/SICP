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
    (- x )))

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
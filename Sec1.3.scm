(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))
		   (+ (* q q) (* 2 p q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

(define (even? n)
  (= (remainder n 2) 0))

(define double (lambda (x) (+ x x)))

(define exp (lambda (a n)
	      (cond ((= n 0) 1)
		    ((even? n) (square (exp a (/ n 2))))
		    (else (* a (exp a (-1+ n)))))))

(define quadratic-root
  (lambda (a b c)
    (/ (+ (- b) (sqrt (- (* b b) (* 4 a c))))
       (* 2 a))))

(quadratic-root 4 6 1)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (cube x) (* x x x))

(integral cube 0 1 0.01)

(integral cube 0 1 0.001)

(define (simp-i f a b n)
  (define h (/ (- b a) n))
  (define (y k) 
    (f (+ a (* k h))))
  (define (simp-fun k)
    (* (cond ((or (= 0 k) (= n k)) 1)
	  ((even? k) 2)
	  ((odd? k)  4))
       (y k)))
  (* (/ h 3)
     (sum simp-fun 0.0 1+ n)))
;Value: simp-i

(simp-i cube 0 1 100)
;Value: .24999999999999992

(integral cube 0 1 .01)
;Value: .24998750000000042

(simp-i cube 0 1 1000)
;Value: .2500000000000003

(integral cube 0 1 0.001)
;Value: .249999875000001

(define (f g)
  (g 2))
;Value: f

(f f)
;The object 2 is not applicable.



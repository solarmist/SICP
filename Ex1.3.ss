;Exercise 1.3.  Define a procedure that takes three numbers as arguments and 
;returns the sum of the squares of the two larger numbers.
"Exercise 1.3"
"Joshua Olson"

;Naive implementation
(define (sq-sum1 a b c)
  (cond ((and (> a b) (> b c)) (+ (* a a) (* b b)))
	((and (> a b) (> c b)) (+ (* a a) (* c c)))
	((and (> b a) (> c a)) (+ (* b b) (* c c)))
	(else (+ (* a a) (* b b)))))

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (eval-biggest a b c function)
  (cond ((and (> a c) (> b c)) (function a b))
	((and (> a b) (> c b)) (function a c))
	((and (> b a) (> c a)) (function b c))
	(else (function a b))))

(define (sq-sum a b c) (eval-biggest a b c sum-of-squares))
 
(sq-sum 2 2 2)
;Value 8

(sq-sum 1 2 3)
;Value 13

(sq-sum 1 2 2)
;Value 8

(sq-sum 33 29 7)
;Value 1930

(sq-sum 3 1 4)
;Value 25

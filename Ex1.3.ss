;Exercise 1.3.  Define a procedure that takes three numbers as arguments and 
;returns the sum of the squares of the two larger numbers.
"Exercise 1.3"
"Joshua Olson"
(define (sq-sum a b c)
  (cond ((and (> a b) (> b c)) (+ (* a a) (* b b)))
	((and (> a b) (> c b)) (+ (* a a) (* c c)))
	((and (> b a) (> c a)) (+ (* b b) (* c c)))
	(else (+ (* a a) (* b b)))))

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

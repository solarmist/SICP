"Exercise 1.7"
"Joshua Olson"
;Exercise 1.7.  The good-enough? test used in computing square roots will not be very 
;effective for finding the square roots of very small numbers. Also, in real computers, 
;arithmetic operations are almost always performed with limited precision. This makes our 
;test inadequate for very large numbers. Explain these statements, with examples showing 
;how the test fails for small and large numbers. An alternative strategy for implementing 
;good-enough? is to watch how guess changes from one iteration to the next and to stop when 
;the change is a very small fraction of the guess. Design a square-root procedure that uses 
;this kind of end test. Does this work better for small and large numbers?

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 100)
;Value: 10.000000000139897

;New version
;Value: 10.032578510960604

(sqrt .00025)
;Value: 3.3869844451165365e-2
;Should be around .015811
;The problem is that the difference for small numbers is often going to be below the 
;threshold of good-enough?

;New version
;Value: 1.5821027443396047e-2

(sqrt 25000000000000000)
;Hangs on such a large number, because eventually the floating point mantissa can't keep 
;track of how small the differences are between one number and the next when there is a 
;very large exponents

;New version
;Value: 158469695.18230283

(define (dist x y)
  (if (< x y) 
      (- y x)
      (- x y)))

;When the ratio of guess and the next guess change by less than x% call it good enough
(define (good-enough? guess x)
  (< (dist (/ guess (improve guess x)) 1) 0.001))

(sqrt 25)
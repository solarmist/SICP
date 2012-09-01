"Exercise 1.13"
"Joshua Olson"

;Exercise 1.13.  Prove that Fib(n) is the closest integer to (phi)^n/sqrt(5), where 
;phi = (1 + sqrt(5))/2. Hint: 
;Let  psi = (1 - sqrt(5))/2. Use induction and the definition of the Fibonacci numbers 
;(see section 1.2.2) 
;to prove that Fib(n) = (phi^n - psi^n)/sqrt(5).

(define phi (/ (+ 1 (sqrt 5)) 2))
(define psi (/ (- 1 (sqrt 5)) 2))
(define Fib
  (lambda (count)
    (define fib-iter
      (lambda (a b count)
	(if (= count 0)
	    b
	    (fib-iter (+ a b) a (-1+ count)))))
    (fib-iter 1 0 count)))

;Note: phi^2 = phi + 1
; and  psi^2 = psi + 1

;Induction: 

;Base cases 
;(fib 0) = (round (/ 1 (sqrt 5))) and (fib 0) = (/ (- 1 1) (sqrt 5)))
;(fib 1) = (round (/ phi (sqrt 5))) and (fib 1) = (/ (- phi psi) (sqrt 5)))

(fib 0)
;Value 0

(fib 1)
;Value 1

(round (/ 1 (sqrt 5)))
;Value 0

(/ (- 1 1) (sqrt 5))
;Value 0

(round (/ phi (sqrt 5)))
;Value 1

(/ (- phi psi) (sqrt 5))
;Value 1

;Assume (fib (- n 1)) = (round (/ (expt phi (- n 1)) (sqrt 5)))
;and    (fib (- n 2)) = (round (/ (expt phi (- n 2)) (sqrt 5)))
;Assume (fib (- n 1)) = (/ (+ (expt phi (- n 1)) (expt psi (- n 1))) (sqrt 5))
;and    (fib (- n 2)) = (/ (+ (expt phi (- n 2)) (expt psi (- n 2))) (sqrt 5))
;Prove (fib n) = (round / (expt phi n)) (sqrt 5))) and
;      (fib n) = (/ (+ (expt phi n) (expt psi n)) (sqrt 5))

;fib (n) = fib (n-1) + fib (n-2)
;(phi^(n-1)-psi^(n-1) + phi^(n-2)-psi^(n-2))/sqrt(5) = (psi^n - phi^n) / sqrt(5)
;(phi^(n-2) (phi + 1) - psi^(n-2) (psi + 1))/sqrt(5) = (psi^n - phi^n) / sqrt(5)
;(phi^(n-2) (phi^2) - psi^(n-2) (psi^2)    )/sqrt(5) = (psi^n - phi^n) / sqrt(5)
;So, fib (n) = (phi^n + psi^n) / sqrt(5)
;Notice that we already proved all base cases and notice that psi^n<0.5 for n>1. Thus,
;by ignoring phi^n we are off by a factor of less than 0.5 for all values of n > 1 for 
;our primary proof.


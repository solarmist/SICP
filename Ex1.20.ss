;; Exercise 1.20.  The process that a procedure generates is of course
;; dependent on the rules used by the interpreter. As an example, consider the
;; iterative gcd procedure given above. Suppose we were to interpret this
;; procedure using normal-order evaluation, as discussed in section 1.1.5. (The
;; normal-order-evaluation rule for if is described in exercise 1.5.) Using the
;; substitution method (for normal order), illustrate the process generated in
;; evaluating (gcd 206 40) and indicate the remainder operations that are
;; actually performed. How many remainder operations are actually performed in
;; the normal-order evaluation of (gcd 206 40)? In the applicative-order
;; evaluation?

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
(remainder 206 40)
(remainder 40 6)
(remainder 6 4)
(remainder 4 2)
2
; Applicative 4

(gcd 206 40)
(gcd 40 (remainder 206 40))
->
(if (= (remainder 206 40) 0); 1 time
    40
    (gcd (remainder 206 40)
	 (remainder 40 (remainder 206 40))))
->
(if (= (remainder 40 (remainder 206 40) 0)); 2 times
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
	 (remainder (remainder 206 40)
		    (remainder 40 (remainder 206 40)))))
->
(if (= (remainder 40 (remainder 206 40) 0)); 3 times
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
	 (remainder (remainder 206 40)
		    (remainder 40 (remainder 206 40)))))
->

;; 18 total calls to remainder
;; This expansion is a pain; see link below
;; http://eli.thegreenplace.net/2007/07/04/sicp-sections-124-125/

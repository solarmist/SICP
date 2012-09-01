"Exercise 1.18"
"Joshua Olson"

;Exercise 1.18.  Using the results of exercises 1.16 and 1.17, devise a procedure that generates an
;iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a
;logarithmic number of steps.

(define (double a)
  (* a 2))

(define (half a)
  (/ a 2))

(define (multi a b)
  (multi-iter a b 0))

(define (multi-iter a b n)
  (cond ((= b 0) n)
	((even? b) (multi-iter (double a) (half b) n))
	(else (multi-iter a (- b 1) (+ a n)))
	)
  )

(multi 3 4)

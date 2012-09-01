"Exercise 1.11"
"Joshua Olson"

; Exercise 1.11.  A function f is defined by the rule that f(n) = n if n < 3 and
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3. Write a procedure that computes f by means
; of a recursive process. Write a procedure that computes f by means of an iterative process.

; Recursive
(define f
  (lambda (n)
    (if (< n 3)
	n
	(+ (f (- n 1))
	   (* 2 (f (- n 2)))
	   (* 3 (f (- n 3)))))))

; Iterative
(define g
  (lambda (n)
    (g-iter 2 1 0 (- n 2))))

(define g-iter
  (lambda (f1 f2 f3 n)
    (if (= n 0)
	f1
	(g-iter (+ f1 (* 2 f2) (* 3 f3) )
		f1
		f2
		(- n 1)
		))))

f
(f 3)
(f 4)
(f 6)

g
(g 3)
(g 4)
(g 6)

;; Exercise 1.21.  Use the smallest-divisor procedure to find the smallest
;; divisor of each of the following numbers: 199, 1999, 19999.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 10)
(smallest-divisor 199)
; prime
(prime? 199)
(smallest-divisor 1999)
; prime
(prime? 1999)
(smallest-divisor 19999)
; 7
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; Exercise 1.25.  Alyssa P. Hacker complains that we went to a lot of extra
;; work in writing expmod. After all, she says, since we already know how
;; to compute exponentials, we could have simply written

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0)

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; Is she correct? Would this procedure serve as well for our fast prime
;; tester? Explain.
;; It would work like she expects for small prime numbers, but we'd quickly
;; reach numbers that couldn't be represented in memory easily, so computing
;; expmod is much more efficient and doesn't require special number constructs
;; for large primes scheme R6RS has this, but R5RS it isn't required.

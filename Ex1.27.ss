;; Exercise 1.27. Demonstrate that the Carmichael numbers listed in footnote 47
;; really do fool the Fermat test. That is, write a procedure that takes an
;; integer n and tests whether an is congruent to a modulo n for every a<n, and
;; try your procedure on the given Carmichael numbers.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (full-fermat n)
  fermat-iter n 2)

(define (fermat-iter n a)


(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
;; Obviously divisible by 5
(fermat-test 2821)
(fermat-test 6601)

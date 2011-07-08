;Exercise 1.4.  Observe that our model of evaluation allows for combinations whose 
;operators are compound expressions. Use this observation to describe the behavior 
;of the following procedure:
"Exercise 1.4"
"Joshua Olson"

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;This procedure will either add b to a if b > 0 or subtract b from a if b <= 0
;The if statement evaluates to a function either + or -

(a-plus-abs-b 1 -8)

(a-plus-abs-b -8 -8)

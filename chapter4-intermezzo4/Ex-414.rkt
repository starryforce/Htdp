#lang htdp/isl+

#| Exercise 414.
As this section illustrates,
gaps in the data representation lead to round-off errors when numbers are mapped to Inexes.
 The problem is, such round-off errors accumulate across computations.

Design add,
a function that adds up n copies of #i1/185. For your examples, use 0 and 1;
for the latter, use a tolerance of 0.0001. What is the result for (add 185)?
What would you expect? What happens if you multiply the result with a large number?

Design sub.
The function counts how often 1/185 can be subtracted from the argument until it is 0.
Use 0 and 1/185 for your examples. What are the expected results?
What are the results for (sub 1) and (sub #i1.0)? What happens in the second case? Why?
|#

; N is one of:
; - 0
; - (add1 N)

(define base #i1/185)
(define tolerance 0.0001)

; N -> Number
; adds up n copies of #i1/185
(define (add n)
  (cond [(= n 0) 0]
        [else (+ #i1/185 (add (sub1 n)))]))

(check-expect (add 0) 0)
(check-within (add 1) base tolerance)
; (check-expect (add 185) 1)
; function result: #i0.9999999999999949
; expect : 1
(check-within (add 185) 1 tolerance)
(check-within (* 10000000000 (add 185)) 10000000000 tolerance)
(check-within (* 100000000000 (add 185)) 100000000000 tolerance)

; when multiply the result with a large number, the result is not accurate any more

; Number -> N
; counts how often 1/185 can be subtracted from num
(define (sub num)
  (cond [(< num 0) (error "not exact")]
        [(= num 0) 0]
        [else (add1 (sub (- num 1/185)))]))

(check-expect (sub 0) 0)
(check-expect (sub 1/185) 1)
(check-expect (sub 1) 185)
(check-error (sub #i1.0) "not exact")

; 
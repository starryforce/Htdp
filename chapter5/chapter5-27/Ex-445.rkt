#lang htdp/isl+

#| Exercise 445.
Consider the following function definition:
; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))
It defines a binomial for which we can determine its roots by hand:
> (poly 2)
0
> (poly 4)
0
Use poly to formulate a check-satisfied test for find-root.
|#

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(check-satisfied (find-root poly 3 6) (lambda (x) (= (poly (round x)) 0)))
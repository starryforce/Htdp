#lang htdp/isl+

#| Exercise 499.
Design an accumulator-style version of product,
the function that computes the product of a list of numbers.
Stop when you have formulated the accumulator invariant and have someone check it.

The performance of product is O(n) where n is the length of the list.
Does the accumulator version improve on this?
|#

; [List-of Number] -> Number
; determine the product of items in alon0
(check-expect (product '(1 2 3)) 6)
(define (product alon0)
  (local (; [List-of Number] Number -> Number
          ; the product of items in alon
          ; accumulator a is the product of the numbers
          ; that alon lacks from alon0
          (define (product/a alon a)
            (cond [(empty? alon) a]
                  [else (product/a (rest alon) (* (first alon) a))])))
    (product/a alon0 1)))

; same O(n) but in reverse order
#lang htdp/isl+

#| Exercise 398.
A linear combination is the sum of many linear terms,
that is, products of variables and numbers.
 The latter are called coefficients in this context. Here are some examples:

In all examples, the coefficient of x is 5, that of y is 17, and the one for z is 3.
If we are given values for variables, we can determine the value of a polynomial.
For example, if x = 10, the value of image is 50; if x = 10 and y = 1, the value of image is 67;
and if x = 10, y = 1, and z = 2, the value of image is 73.

There are many different representations of linear combinations.
We could, for example, represent them with functions.
An alternative representation is a list of its coefficients.
The above combinations would be represented as:
(list 5)
(list 5 17)
(list 5 17 3)
This choice of representation assumes a fixed order of variables.
Design value. The function consumes two equally long lists:
a linear combination and a list of variable values.
It produces the value of the combination for these values. 
|#

; [List-of Number] [List-of Number] -> Number
; produces the value of the combination for these values
(define (value alc alov)
  (cond [(empty? alc) 0]
        [else (+ (* (first alc) (first alov))
                 (value (rest alc) (rest alov)))]))

(check-expect (value (list 5) (list 10)) 50)
(check-expect (value (list 5 17) (list 10 1)) 67)
(check-expect (value (list 5 17 3) (list 10 1 2)) 73)


#lang htdp/isl+

#| Exercise 465.
Design subtract. The function consumes two Equations of equal length.
It “subtracts” a multiple of the second equation from the first, item by item,
so that the resulting Equation has a 0 in the first position.
Since the leading coefficient is known to be 0,
subtract returns the rest of the list that results from the subtractions.
|#

; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side

; Equation Equation -> Equation
; assume e1 and e2 are of same length
(check-expect (subtract (list 2 5 12 31) (list 2 2 3 10)) (list 3 9 21))
(check-expect (subtract (list 4 1 -2 1) (list 2 2 3 10)) (list -3 -8 -19))
(define (subtract e1 e2)
  (local ((define t (/ (first e1) (first e2))))
    (rest (map (lambda (n1 n2) (- n1 (* n2 t))) e1 e2))))
  


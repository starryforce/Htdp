#lang htdp/isl+

#| Exercise 462.
Design the function check-solution.
It consumes an SOE and a Solution.
Its result is #true if plugging in the numbers from the Solution
for the variables in the Equations of the SOE
produces equal left-hand-side values and right-hand-side values;
otherwise the function produces #false.
Use check-solution to formulate tests with check-satisfied.

Hint Design the function plug-in first.
It consumes the left-hand side of an Equation and a Solution
and calculates out the value of the left-hand side
when the numbers from the solution are plugged in for the variables.
|#

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; SOE Solution -> Boolean
; determine if s is the right solution for soe
(define (check-solution soe s)
  (local (; Equation -> Boolean
          ; determine if equation is correct for s
          (define (correct? e)
            (= (plug-in (lhs e) s) (rhs e))))
    (andmap correct? soe)))

(check-expect (check-solution M S) #t)
(check-expect (check-solution M '(1 0 1)) #f)
(check-expect (check-solution '((2 2 3 10) (0 3 9 21) (0 0 1 2)) '(1 1 2)) #t)

; [List-of Number] Solution -> Number
; calculates out the value of the left-hand side
; when the numbers from s are plugged in for the variables
; assume l & s are of equal length
(define (plug-in l s)
  (cond [(empty? s) 0]
        [else (+ (* (first l) (first s))
                 (plug-in (rest l) (rest s)))]))

(check-expect (plug-in (lhs (list 2 2  3 10)) S) 10)
(check-expect (plug-in (lhs (list 2 5 12 31)) S) 31)
(check-expect (plug-in (lhs (list 4 1 -2  1)) S) 1)

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))
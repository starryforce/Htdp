#lang htdp/isl+

#| Exercise 466.
Here is a representation for triangular SOEs:
Design the triangulate algorithm:

Turn the above example into a test and
spell out explicit answers for the four questions based on our loose description.
Do not yet deal with the termination step of the design recipe.
|#

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations 
(define (triangulate M)
  (cond [(= (length M) 1) M]
        [else (cons (first M) 
                    (triangulate (map (lambda (x) (subtract x (first M))) (rest M))))]))

(check-expect (triangulate '((1 2))) '((1 2)))
(check-expect (triangulate M) (list (list 2 2  3 10)
                                    (list   3  9 21)
                                    (list      1  2)))

(define (subtract e1 e2)
  (local ((define t (/ (first e1) (first e2))))
    (rest (map (lambda (n1 n2) (- n1 (* n2 t))) e1 e2))))

; 1. What is a trivially solvable problem?
; single item list 

; 2. How are trivial problems solved?
; itself

; 3. How does the algorithm generate new problems that are more easily solvable than the original one?
; Is there one new problem that we generate or are there several?
; substract the first item from every other item to get a new smaller SOE

; 4 .Is the solution of the given problem the same as the solution of (one of) the new problems?
; Or, do we need to combine the solutions to create a solution for the original problem?
; And, if so, do we need anything from the original problem data?
; then combine the smaller SOE with the subtracted item. 

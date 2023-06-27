#lang htdp/isl+

#| Exercise 467.
Revise the algorithm triangulate from exercise 466
so that it rotates the equations first to find one with a leading coefficient
that is not 0 before it subtracts the first equation from the remaining ones.

Does this algorithm terminate for all possible systems of equations?

Hint The following expression rotates a non-empty list L:
(append (rest L) (list (first L)))

Explain why.

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

(define M1
  (list (list 2  3  3 8)
        (list 2  3 -2 3)
        (list 4 -2  2 4)))

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations 
(define (triangulate M)
  (local ((define base (first M)))
    (cond [(= (length M) 1) (list (filter (lambda (x) (not (zero? x))) (first M)))]
          [(= (first base) 0) (triangulate (append (rest M) (list (first M))))]
          [else (cons base
                      (triangulate (map (lambda (x) (subtract x base)) (rest M))))])))

(check-expect (triangulate '((1 2))) '((1 2)))
(check-expect (triangulate M) (list (list 2 2  3 10)
                                    (list   3  9 21)
                                    (list      1  2)))
(check-expect (triangulate M1) (list (list 2  3  3   8)
                                     (list   -8 -4 -12)
                                     (list      -5  -5)))

(define (subtract e1 e2)
  (local ((define t (/ (first e1) (first e2))))
    (rest (map (lambda (n1 n2) (- n1 (* n2 t))) e1 e2))))

; because it move the first item to the last of the list,
; so every item come 1 position upward.
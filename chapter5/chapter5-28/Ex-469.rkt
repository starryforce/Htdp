#lang htdp/isl+

; Exercise 469.
; Design the solve function. It consumes triangular SOEs and produces a solution.

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix
 
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
(define TM
  (list (list 2 2  3 10)
      (list   3  9 21)
      (list      1  2)))
(define TM1 ; a TM 
  (list (list 2  3  3   8) 
        (list   -8 -4 -12)
        (list      -5  -5)))
 
(define S '(1 1 2)) ; a Solution

; TM -> Solution
; consumes triangular SOEs and produces a solution.
(check-expect (solve TM) S)
(check-expect (solve TM1) '(1 1 1))
(define (solve-o t)
  (cond [(empty? t) '()]
        [else (local ((define rs (solve (rest t))))
                (cons (solve-equation (first t) rs)
                      rs))]))
; TM -> Solution
; consumes triangular SOEs and produces a solution.
(define (solve t)
  (foldr (lambda (e base) (cons (solve-equation e base) base))
         '()
         t))

; Equation [rest Solution] -> Number;
; e is a equation, s represents a solution for the last n variables
(check-expect (solve-equation '(2 3 3 8) '(1 1)) 1)
(check-expect (solve-equation '(-8 -4 -12) '(1)) 1)
(check-expect (solve-equation '(2 2 3 10) '(1 2)) 1)
(check-expect (solve-equation '(2 3) '()) 1.5)
(define (solve-equation e s)
  (local ((define l (lhs e))
          (define r (rhs e)))
    (cond [(= (length l) 1) (/ r (first l))]
          [else (local ((define c1 (first l))
                        (define osum (foldr
                                      (lambda (x y base) (+ base (* x y)))
                                      0
                                      (rest l)
                                      s))
                        )
                  (/ (- r osum) c1))])))

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

#lang htdp/isl+

#| Exercise 470.
Define gauss, which combines the triangulate function from exercise 468 and the solve function from exercise 469.
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
(define M2
  (list (list 2 2 2 6)
        (list 2 2 4 8)
        (list 2 2 1 2)))

; A Solution is a [List-of Number]
(define S '(1 1 2)) ; a Solution

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

(define NO_SOLUTION "no solution")

; SOE -> TM
; triangulates the given system of equations 
(define (triangulate M)
  (local ((define base (first M))
          (; SOE -> Boolean
           ; determine if M has no solution
           ; leading coefficients are all 0
           define (nosolution? M)
            (andmap (lambda (x) (zero? (first x))) M)))
    (cond [(= (length M) 1) (list (filter (lambda (x) (not (zero? x))) (first M)))]
          [(nosolution? M) (error NO_SOLUTION)]
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
(check-error (triangulate M2) NO_SOLUTION)

(define (subtract e1 e2)
  (local ((define t (/ (first e1) (first e2))))
    (rest (map (lambda (n1 n2) (- n1 (* n2 t))) e1 e2))))
 

(define TM
  (list (list 2 2  3 10)
      (list   3  9 21)
      (list      1  2)))
(define TM1 ; a TM 
  (list (list 2  3  3   8) 
        (list   -8 -4 -12)
        (list      -5  -5)))

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

; SOE -> Solution
; 
(define (gauss m) (solve (triangulate M)))

(check-expect (gauss M) S)

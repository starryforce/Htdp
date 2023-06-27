#lang htdp/isl+

#| Exercise 456.
Design root-of-tangent,
a function that maps f and r1 to the root of the tangent through (r1,(f r1)).
|#

(define ε 0.00000001)

(define (f0 x) 3)
(define (f1 x) x)
(define (f2 x) (+ 15 (* 3 x)))

; [Number -> Number] Number -> Number
; determine the slope of f at r1
(define (slope f r1)
  (* (/ 1 (* 2 ε))
     (- (f (+ r1 ε))
        (f (- r1 ε)))))

(check-expect (slope f0 3) 0)
(check-expect (slope f1 9) 1)
(check-expect (slope f2 200) 3)

; [Number -> Number] Number -> Number
; determine the root of the tangent through (r1,(f r1))
(define (root-of-tangent f r1)
  (- r1
     (/ (f r1)
        (if (zero? (slope f r1))
            (error "no root")
            (slope f r1)))))

(check-error (root-of-tangent f0 3) "no root")
(check-expect (root-of-tangent f1 9) 0)
(check-expect (root-of-tangent f2 200) -5)
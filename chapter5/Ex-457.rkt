#lang htdp/isl+

#| Exercise 457. Design the function double-amount,
which computes how many months it takes to double a given amount of money
when a savings account pays interest at a fixed rate on a monthly basis.

Domain Knowledge With a minor algebraic manipulation,
you can show that the given amount is irrelevant.
Only the interest rate matters.
Also domain experts know that doubling occurs after
roughly 72/r month as long as the interest rate r is “small.”
|#

(define rate 0.001)

; Number N -> Number
; calc the total money after m month with monthly basis is rate,
; which initial capital is sc
(define (money c m)
  (cond [(<= m 0) c]
        [else (* (add1 rate) (money c (sub1 m)))]))

(check-expect (money 10000 1) (* 10000 (+ 1 rate)))
(check-expect (money 10030 1) (* 10030 (+ 1 rate)))
(check-expect (money 10000 2) (* (* 10000 (+ 1 rate)) (+ 1 rate)))

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

; Number -> Number
(define (poly x) (* (- x 2) (- x 4)))

; [Number -> Number] Number -> Number
; finds a number r such that (<= (abs (f r)) ε)
 
(check-within (newton poly 1) 2 ε)
(check-within (newton poly 3.5) 4 ε)
 
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) ε) r1]
    [else (newton f (root-of-tangent f r1))]))

; Number -> Number
; computes how many months it takes to double
; a given amount of money when a savings account pays interest at a fixed rate on a monthly basis.
(define (double-amount guess)
  (local ((define (f m)
            (abs (- (money 100 m) (* 100 2) ))))
    (newton f guess)))
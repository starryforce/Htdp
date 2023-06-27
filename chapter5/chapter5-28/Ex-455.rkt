#lang htdp/isl+

#| Exercise 455.
Translate this mathematical formula into the ISL+ function slope,
which maps function f and a number r1 to the slope of f at r1.
Assume that ε is a global constant.
For your examples, use functions whose exact slope you can figure out,
say, horizontal lines, linear functions,
and perhaps polynomials if you know some calculus.
|#
(define ε 0.00000001)

(define (f0 x) 3)
(define (f1 x) x)
(define (f2 x) (+ 5 (* 3 x)))

; [Number -> Number] Number -> Number
; determine the slope of f at r1
(define (slope f r1)
  (* (/ 1 (* 2 ε))
     (- (f (+ r1 ε))
        (f (- r1 ε)))))

(check-expect (slope f0 3) 0)
(check-expect (slope f1 9) 1)
(check-expect (slope f2 200) 3)
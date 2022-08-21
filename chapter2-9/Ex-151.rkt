#lang htdp/bsl

; N Number -> Number
; multiplies n with a number x without using *.
(define (multiply n x)
  (cond [(zero? n) 0]
        [(positive? n) (+ (multiply (sub1 n) x) x)]
        ))

(check-expect (multiply 0 8) (* 0 8))
(check-expect (multiply 5 0) (* 5 0))
(check-expect (multiply 5 3) (* 5 3))
(check-expect (multiply 5 -2.5) (* 5 -2.5))


(multiply 3 2)
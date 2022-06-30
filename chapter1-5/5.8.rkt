#lang htdp/bsl

(define-struct r3 [x y z])
; A R3 is a structure:
; (make-r3 Number Number Number)
(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))

; R3 -> Number
; determines the distance of r to the origin
(define (r3-distance-to-0 r) (sqrt (+ (* (r3-x r) (r3-x r)) (* (r3-y r) (r3-y r)) (* (r3-z r) (r3-z r)))))

; Epsilon ??
(check-within (r3-distance-to-0 ex1) (sqrt (+ (* 1 1) (* 2 2) (* 13 13))) 0.000001)
(check-within (r3-distance-to-0 ex2) (sqrt (+ (* -1 -1) (* 0 0) (* 3 3))) 0.000001)
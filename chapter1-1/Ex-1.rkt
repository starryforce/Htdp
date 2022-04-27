#lang htdp/bsl

(define x 12)
(define y 5)

(define (distance x y)
  (sqrt (+ (* x x) (* y y))))

(distance x y)
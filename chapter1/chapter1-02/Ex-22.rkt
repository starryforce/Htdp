#lang htdp/bsl


(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))
(distance-to-origin 3 4)

;(sqrt (+ (sqr 3) (sqr 4)))
;(sqrt (+ 9 (sqr 4)))
;(sqrt (+ 9 16))
;(sqrt 25)
;5
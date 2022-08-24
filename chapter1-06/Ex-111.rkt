#lang htdp/bsl

(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector


; Any Any -> Vec
; generate a Vec with x y,
; if x and y are both positive number
(define (checked-make-vec x y)
  (cond [(and (and (number? x) (> x 0)) (and (number? x) (> x 0))) (make-vec x y)]
        [else (error "both should be positive numbers")]))
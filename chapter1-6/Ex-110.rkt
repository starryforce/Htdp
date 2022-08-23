#lang htdp/bsl


; Number -> Number
; computes the area of a disk with radius r
(define (area-of-disk r)
  (* 3.14 (* r r)))

; Any -> Number
; computes the area of a disk with radius v, 
; if v is a positive number
(define (checked-area-of-disk v)
  (cond
    [(and (number? v) (> v 0)) (area-of-disk v)]
    [else (error "area-of-disk: positive number expected")]))
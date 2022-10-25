#lang htdp/isl

; Nelon -> Number
; determines the smallest number on l
(define (sup l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define largest-in-rest (sup (rest l))))
       (cond [(> (first l) largest-in-rest)
              (first l)]
             [else largest-in-rest]))]))

(sup (list 2 1 3))
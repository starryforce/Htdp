#lang htdp/isl

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf (rest l))))
       (cond [(< (first l) smallest-in-rest)
              (first l)]
             [else smallest-in-rest]))]))

(inf (list 2 1 3))
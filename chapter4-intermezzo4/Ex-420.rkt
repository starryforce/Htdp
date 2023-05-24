#lang htdp/isl+

; [List-of Number] -> Number
; calc the sum of numbers in l
(define (sum l)
  (foldl + 0 l))

(define (oscillate n)
  (local ((define (O i)
            (cond
              [(> i n) '()]
              [else
               (cons (expt #i-0.99 i) (O (+ i 1)))])))
    (O 1)))

(sum (oscillate #i1000.0))

(sum (reverse (oscillate #i1000.0)))
#lang htdp/isl+

#| Exercise 501.
Design an accumulator-style version of add-to-pi.
The function adds a natural number to pi without using +:
|#

; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n0)
  (local (; N ??? -> Number
          ; adds n to pi
          ; accumulator a is pi plus (n0 - n)
          (define (add-to-pi/a n a)
            (cond
              [(zero? n) a]
              [else (add-to-pi/a (sub1 n) (add1 a))])))
    (add-to-pi/a n0 pi)))

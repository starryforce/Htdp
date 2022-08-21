#lang htdp/bsl

; N -> Number
; computes (+ n pi) without using +

(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond [(zero? n) pi]
        [(positive? n)
         (add1 (add-to-pi (sub1 n)))]))

(define (add-to-x n x)
  (cond [(zero? n) x]
        [(positive? n)
         (add1 (add-to-x (sub1 n) x))]))

(check-expect (add-to-x 6 2.8) 8.8)
(check-expect (add-to-x 6 4) 10)

; because pi is not an accurate number
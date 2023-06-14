#lang htdp/isl+

(define ε 0.1)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 
 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)
 
(define (integrate-kepler f a b)
  (* (/ 1 2) 
     (- b a) (+ (f a) (f b))))


; the last test case failed, 500
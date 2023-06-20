#lang htdp/isl+

#| Exercise 460.
Develop the algorithm integrate-dc,
which integrates a function f between the boundaries a and b
using a divide-and-conquer strategy.
Use Kepler’s method when the interval is sufficiently small.
|#

(define ε 0.1)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; divide the area into two parts of the same size, and calc
; the same of them,
; termination each time the range is half of the origin range
; assume (< a b) holds
 
(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)
 
(define (integrate-dc f a b)
  (local ((define midpoint (+ a (/ (- b a) 2))))
    (cond [(< (- b a) ε) (integrate-kepler f a b)]
          [else (+ (integrate-dc f a midpoint)
                   (integrate-dc f midpoint b))])))


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
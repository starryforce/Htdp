#lang htdp/isl+

(define ε 0.01)
; the number of rectangles
(define R (expt 10 6))

 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 
 
(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)
 
(define (integrate-rectangles f a b)
  (local (; the width of each rectangle
          (define W (/ (- b a) R))
          (define S (/ W 2))
          ; determine the area of ith rectangle
          (define (fn i)
            (* W (f (+ a (* i W) S))))
          ; calc the first n rectangles's total area
          (define (area n)
            (cond [(zero? n) (fn 0)]
                  [else (+ (fn n) (area (sub1 n)))])))
    (area R)))

; when R equals to (expt 10 5), all test cases passed.
; when R equals to (expt 10 6), all test cases passed if ε equals to 0.01
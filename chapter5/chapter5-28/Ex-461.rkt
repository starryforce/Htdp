#lang htdp/isl+

#| Exercise 461.
Design integrate-adaptive.
That is, turn the recursive process description into an ISL+ algorithm.
Make sure to adapt the test cases from figure 165 to this use.

Do not discuss the termination of integrate-adaptive.

Does integrate-adaptive always compute a better answer
than either integrate-kepler or integrate-rectangles from exercise 459?
Which aspect is integrate-adaptive guaranteed to improve?
|#

(define ε 0.1)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; divide the area into two parts of the same size and calc
; the same of them,
; termination each time the range is half of the origin range
; assume (< a b) holds
 
(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)
 
(define (integrate-adaptive f a b)
  (local ((define midpoint (+ a (/ (- b a) 2)))
          (define left (integrate-kepler f a midpoint))
          (define right (integrate-kepler f midpoint b))
          (define diff (abs (- left right)))
          )
    (cond [(< diff (*  ε (- b a))) (integrate-kepler f a b)]
          [else (+ (integrate-adaptive f a midpoint)
                   (integrate-adaptive f midpoint b))])))


; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 
 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
 
(define (integrate-kepler f a b)
  (* (/ 1 2) 
     (- b a) (+ (f a) (f b))))


; guaranteed perform time.
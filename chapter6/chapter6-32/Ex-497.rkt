#lang htdp/isl+

#| Exercise 497.
Like sum, !.v1 performs the primitive computations,
multiplication in this case, in reverse order.
Surprisingly, this affects the performance of the function in a negative manner.

Measure how long it takes to evaluate (!.v1 20) 1,000 times.
Recall that (time an-expression) function determines how long it takes to run an-expression.
|#

; N -> N 
; computes (* n (- n 1) (- n 2) ... 1)
(check-expect (!.v1 3) 6)
(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))


(define (main x)
  (build-list 10000 (lambda (x) (!.v1 20))))

(time (main 0))
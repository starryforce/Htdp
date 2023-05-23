#lang htdp/isl+

#| Exercise 416.
ISL+ uses #i0.0 to approximate underflow.
Determine the smallest integer n such that
(expt #i10.0 n) is still an inexact ISL+ number
and (expt #i10. (- n 1)) is approximated with 0.
Hint Use a function to compute n.
Consider abstracting over this function and the solution of exercise 415. 
|#

;; N is one of:
;; - 0
;; - (add1 N)


; N -> N
; Determines the smallest integer n such that 
; - (expt #i10.0 n) is still an inexact ISL+ number
; - (expt #i10. (- n 1)) is approximated with 0
(define (search n)
  (cond [(and (not (zero? (expt #i10.0 n)))
             (zero? (expt #i10. (- n 1)))) n]
        [else (search (sub1 n))]))

(search 0)
; -323
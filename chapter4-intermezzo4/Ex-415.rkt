#lang htdp/isl+

#| Exercise 415.
ISL+ uses +inf.0 to deal with overflow.
Determine the integer n such that
(expt #i10.0 n) is an inexact number
while (expt #i10. (+ n 1)) is approximated with +inf.0.
Hint Design a function to compute n. 
|#

;; N is one of:
;; - 0
;; - (add1 N)


; N -> N
; Determine the integer n such that
; - (expt #i10.0 n) is an inexact number
; - (expt #i10. (+ n 1)) is approximated with +inf.0.
(define (search n)
  (cond [(and (integer? (expt #i10.0 n))
              (not (integer? (expt #i10.0 (add1 n))))) n]
        [else (search (add1 n))]))

(search 0)
; 308
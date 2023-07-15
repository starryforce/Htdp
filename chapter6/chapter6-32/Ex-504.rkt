#lang htdp/isl+

#| Exercise 504.
Design to10. It consumes a list of digits and produces the corresponding number.
The first item on the list is the most significant digit.
Hence, when applied to '(1 0 2), it produces 102.
|#

; [List-of Number] -> Number
; produces the corresponding number
(check-expect (to10 '(1 0 2)) 102)
(define (to10 alon0)
  (local (; [List-of Number] ??? -> Number
          ; produces the corresponding number of alon0
          ; accumulotar a is corresponding
          ; number of numbers in  alon lacks from alon0
          (define (to10/a alon a)
            (cond [(empty? alon) a]
                  [else (to10/a (rest alon)
                                (+ a (* (first alon) (expt 10 (sub1 (length alon))))))])))
    (to10/a alon0 0)))
#lang htdp/isl+

; Exercise 506.
; Design an accumulator-style version of map.

; [List-of X] [X -> Y] -> Y
(check-expect (map.v2 add1 '(1 2 3)) '(2 3 4))
(define (map.v2 fn l0)
  (local (; [List-of X] ??? -> [List-of Y]
          ; accumulator a is the result of items l lacks from l0
          (define (map.v2/a l a)
            (cond [(empty? l) (reverse a)]
                  [else (map.v2/a (rest l) (cons (fn (first l)) a))])))
    (map.v2/a l0 '())))
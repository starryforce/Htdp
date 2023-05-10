#lang htdp/isl+

#| Exercise 394.
Design merge. The function consumes two lists of numbers,sorted in ascending order.
It produces a single sorted list of numbers that contains all the numbers on both inputs lists.
A number occurs in the output as many times as it occurs on the two input lists together.
|#

; [List-of Number] [List-of Number] -> [List-of Number]
; produces a sorted list of number with all numbers in alon1 & alon2
(define (merge alon1 alon2)
  (cond [(empty? alon1) alon2]
        [else (merge (rest alon1)
                     (insert alon2 (first alon1)))]))

(check-expect (merge '() '()) '())
(check-expect (merge '(8) '()) '(8))
(check-expect (merge '() '(9)) '(9))
(check-expect (merge '(8) '(9)) '(8 9))
(check-expect (merge '(6 8) '(7 9)) '(6 7 8 9))

; [List-of Number] Number -> [List-of Number]
; insert n into alon, still keeping alon still sorted in ascending order
(define (insert alon n)
  (cond [(empty? alon) (cons n '())]
        [else (cond [(< n (first alon)) (cons n alon)]
                    [else (cons (first alon) (insert (rest alon) n))])]))

(check-expect (insert '() 1) '(1))
(check-expect (insert '(2) 1) '(1 2))
(check-expect (insert '(1 3) 0) '(0 1 3))
(check-expect (insert '(1 3) 1) '(1 1 3))
(check-expect (insert '(1 3) 2) '(1 2 3))
(check-expect (insert '(1 3) 3) '(1 3 3))
(check-expect (insert '(1 3) 4) '(1 3 4))

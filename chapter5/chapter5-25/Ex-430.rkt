#lang htdp/isl+

#| Exercise 430.
Develop a variant of quick-sort< that uses only one comparison function, say, <.
Its partitioning step divides the given list alon into a list that
contains the items of alon smaller than the pivot and another one with those that are not smaller.

Use local to package up the program as a single function.
 Abstract this function so that it consumes a list and a comparison function.
|#

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon))
                  (define (left-side i) (< i pivot))
                  (define (right-side i) (not (left-side i))))
            (append (quick-sort< (filter left-side (rest alon)))
                    (list pivot)
                    (quick-sort< (filter right-side (rest alon)))))]))
 

(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort< (list 11 8 14 8 7 8 14)) (list 7 8 8 8 11 14 14))


(define (quick-sort alon cmp)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon))
                  (define (meet? i) (cmp i pivot))
                  (define (not-meet? i) (not (meet? i))))
            (append (quick-sort (filter meet? (rest alon)) cmp)
                    (list pivot)
                    (quick-sort (filter not-meet? (rest alon)) cmp)))]))

(check-expect (quick-sort (list 11 8 14 7) <) (list 7 8 11 14))
(check-expect (quick-sort (list 11 8 14 8 7 8 14) <) (list 7 8 8 8 11 14 14))
(check-expect (quick-sort (list 11 8 14 7) >) (list 14 11 8 7))
(check-expect (quick-sort (list 11 8 14 8 7 8 14) >) (list 14 14 11 8 8 8 7))

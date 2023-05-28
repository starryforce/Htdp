#lang htdp/isl+

#| Exercise 435.
When you worked on exercise 430 or exercise 428,
you may have produced looping solutions.
Similarly, exercise 434 actually reveals
how brittle the termination argument is for quick-sort<.
In all cases, the argument relies on the idea that
smallers and largers produce lists that are maximally as long as the given list,
and on our understanding that neither includes the given pivot in the result.

Based on this explanation,
modify the definition of quick-sort< so that
both functions receive lists that are shorter than the given one. 
|#

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
; termination the recusion in the body process alon except pivot
; this guareteen the size of input is at least 1 smaller than origin argument
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

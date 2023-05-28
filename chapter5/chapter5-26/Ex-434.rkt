#lang htdp/isl+

#| Exercise 434. Consider the following definition of smallers,
one of the two “problem generators” for quick-sort<:

What can go wrong when this version is used with
the quick-sort< definition from Recursion that Ignores Structure?
|#

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers l n)
  (cond
    [(empty? l) '()]
    [else (if (<= (first l) n)
              (cons (first l) (smallers (rest l) n))
              (smallers (rest l) n))]))

(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (quick-sort< (list 11 8 14 8 7 8 14)) (list 7 8 8 8 11 14 14))

; (quick-sort< (smallers alon pivot)) this step the size of the input could
; be same with origin alon
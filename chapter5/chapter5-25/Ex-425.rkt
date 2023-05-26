#lang htdp/isl+

#| Exercise 425.
Articulate purpose statements for smallers and largers in figure 149.
|#


; [List-of Number] Number -> [List-of Number]
; pick all numbers larger than n in alon.
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
; pick all numbers smaller than n in alon.
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))
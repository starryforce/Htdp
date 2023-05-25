#lang htdp/isl+

#| Exercise 426.
Complete the hand-evaluation from above.
A close inspection of the evaluation suggests an additional trivial case for quick-sort<.
Every time quick-sort< consumes a list of one item, it returns it as is.
After all, the sorted version of a list of one item is the list itself.

Modify quick-sort< to take advantage of this observation.
Evaluate the example again.
How many steps does the revised algorithm save?
|#

#|
...
==
(append (append (list 7)
                (list 8)
                '())
        (list 11)
        (quick-sort< (list 14)))
==
(append (list 7 8)
        (list 11)
        (quick-sort< (list 14)))
==
(append (list 7 8)
        (list 11)
        (append (quick-sort< '())
                (list 14)
                (quick-sort< '())))
==
(append (list 7 8)
        (list 11)
        (append '()
                (list 14)
                '()))
==
(append (list 7 8)
        (list 11)
        (list 14))
==
(append (list 7 8 11 14))
|#

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
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
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

(check-expect (quick-sort< (list 11 8 14 7)) (list 7 8 11 14))

(quick-sort< (list 11 8 14 7))
==
(append (quick-sort< (list 8 7))
        (list 11)
        (quick-sort< (list 14)))
==
(append (append (quick-sort< (list 7))
                (list 8)
                (quick-sort< '()))
        (list 11)
        (quick-sort< (list 14)))
==
(append (append (list 7)
                (list 8)
                (quick-sort< '()))
        (list 11)
        (quick-sort< (list 14)))
==
(append (append (list 7)
                (list 8)
                '())
        (list 11)
        (quick-sort< (list 14)))
==
(append (list 7 8)
        (list 11)
        (quick-sort< (list 14)))
==
(append (list 7 8)
        (list 11)
        (list 14))
==
(append (list 7 8 11 14))

        
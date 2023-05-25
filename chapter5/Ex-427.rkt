#lang htdp/isl+

#| Exercise 427.
While quick-sort< quickly reduces the size of the problem in many cases,
 it is inappropriately slow for small problems.
Hence people use quick-sort< to reduce the size of the problem and
switch to a different sort function when the list is small enough.

Develop a version of quick-sort< that uses sort<
(an appropriately adapted variant of sort> from Auxiliary Functions that Recur)
if the length of the input is below some threshold.
|#

(define threshold 4)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(< (length alon) threshold) (sort< alon)]
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
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))
(check-expect (quick-sort< '(17 38 77 74 98 33 81 31 8 90 48 2 63 50 32 56 23 51 85 73 93 21 25 15 41 70 37 100 44 26))
                           '(2 8 15 17 21 23 25 26 31 32 33 37 38 41 44 48 50 51 56 63 70 73 74 77 81 85 90 93 98 100))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))
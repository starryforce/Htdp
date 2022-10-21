#lang htdp/isl

(define ex1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))
(define ex2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25))

; CompareF Nelon -> Number
; pick a number from l by CompareF
(define (pick R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (R (first l)
            (pick R (rest l)))
         (first l)
         (pick R (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf-1 l) (pick < l))
(check-expect (inf-1 ex1) 1)
(check-expect (inf-1 ex2) 1)

; Nelon -> Number
; determines the largest 
; number on l
(define (sup-1 l) (pick > l))
(check-expect (sup-1 ex1) 25)
(check-expect (sup-1 ex2) 25)
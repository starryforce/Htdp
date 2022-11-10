#lang htdp/isl

(define ex1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))
(define ex2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min (first l)
          (inf (rest l)))]))
(check-expect (inf ex1) 1)
(check-expect (inf ex2) 1)

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (max (first l)
          (sup (rest l)))]))
(check-expect (sup ex1) 25)
(check-expect (sup ex2) 25)

; CompareF Nelon -> Number
; pick a number on l by method R
(define (pick R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (R (first l)
          (pick R (rest l)))]))

(define (inf-1 l) (pick min l))
(check-expect (inf-1 ex1) 1)
(check-expect (inf-1 ex2) 1)

(define (sup-1 l) (pick max l))
(check-expect (sup-1 ex1) 25)
(check-expect (sup-1 ex2) 25)

; This version use a compare stack to delay the comparison
; and the prev version 

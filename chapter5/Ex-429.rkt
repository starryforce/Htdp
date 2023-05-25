#lang htdp/isl+

; Exercise 429. Use filter to define smallers and largers.

; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (filter (lambda (i) (> i n)) alon))

(check-expect (largers (list 11 8 14 7) 11) (list 14))

; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (filter (lambda (i) (< i n)) alon))

(check-expect (smallers (list 11 8 14 7) 11) (list 8 7))
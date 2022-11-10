#lang htdp/isl

(define ex0 '())
(define ex1 (list 1))
(define ex2 (list 1 2 3 4 5))

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

(check-expect (sum ex0) 0)
(check-expect (sum ex1) 1)
(check-expect (sum ex2) 15)

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

(check-expect (product ex0) 1)
(check-expect (product ex1) 1)
(check-expect (product ex2) 120)

(define (fold1 l initial g)
  (cond
    [(empty? l) initial]
    [else
     (g (first l)
        (fold1 (rest l) initial g))]))

(define (sum-from-abstract l)
  (fold1 l 0 +))

(check-expect (sum-from-abstract ex0) 0)
(check-expect (sum-from-abstract ex1) 1)
(check-expect (sum-from-abstract ex2) 15)

(define (product-from-abstract l)
  (fold1 l 1 *))

(check-expect (product-from-abstract ex0) 1)
(check-expect (product-from-abstract ex1) 1)
(check-expect (product-from-abstract ex2) 120)


#lang htdp/bsl

; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)

(define ex1 '())
(define ex2 (cons 20.22 ex1))
(define ex3 (cons 15.34 ex2))

; List-of-amounts -> Number
; calc sum of the list l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (sum (rest l)))]))

(check-expect (sum ex1) 0)
(check-expect (sum ex2) 20.22)
(check-expect (sum ex3) 35.56)



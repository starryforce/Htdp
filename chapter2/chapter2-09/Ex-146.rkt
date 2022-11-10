#lang htdp/bsl

(define ex1 (cons 12 '()))
(define ex2 (cons 122 (cons 44 '())))
(define ex3 (cons 123 ex2))

; NEList-of-temperatures -> Number
; count how many items in nlot
(define (how-many nlot)
  (cond [(empty? (rest nlot)) 1]
        [else (+ 1 (how-many (rest nlot)))]))

(check-expect (how-many ex1) 1)
(check-expect (how-many ex2) 2)
(check-expect (how-many ex3) 3)

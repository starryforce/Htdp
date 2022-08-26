#lang htdp/bsl

; Lop (short for list of posns) is one of:
; - '()
; - (cons Posn LOP)
; interpretation an instance of Lop
; represents a list of Posns

(define ex0 '())
(define ex1 (cons (make-posn 10 20) '()))
(define ex2 (cons (make-posn 20 50) ex1))

; Lop -> Number
; produces the sum of all of its x-coordinates in alop
(define (sum alop)
  (cond [(empty? alop) 0]
        [else (+ (posn-x (first alop)) (sum (rest alop)))]))

(check-expect (sum ex0) 0)
(check-expect (sum ex1) 10)
(check-expect (sum ex2) 30)

#lang htdp/bsl

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

(define ex0 '())
(define ex1 (cons 5 '()))
(define ex2 (cons -1 '()))
(define ex3 (cons -1 ex1))
(define ex4 (cons 1 ex1))
(define ex5 (cons 0 ex2))
(define ex6 (cons 1 ex2))

; +List-of-numbers-> Boolean
; check is all the numbers in the alon is positive
(define (pos? alon)
  (cond
    [(empty? alon) #true]
    [else (cond [(<= (first alon) 0) #false]
                [else (pos? (rest alon))])]))

(check-expect (pos? ex0) #true)
(check-expect (pos? ex1) #true)
(check-expect (pos? ex2) #false)
(check-expect (pos? ex3) #false)
(check-expect (pos? ex4) #true)
(check-expect (pos? ex5) #false)
(check-expect (pos? ex5) #false)

; List-of-amounts -> Number
; calc sum of the list l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (sum (rest l)))]))


; List-of-numbers -> Number
; Else -> Error
; when alon is List-of-amounts, calc sum of it;
; if not, signal an error
(define (checked-sum alon)
  (cond
    [(pos? alon) (sum alon)]
    [else (error "alon is not List-of-numbers")]))

(check-expect (checked-sum ex0) 0)
(check-expect (checked-sum ex1) 5)
(check-error (checked-sum ex2))
(check-error (checked-sum ex3))
(check-expect (checked-sum ex4) 6)
(check-error (checked-sum ex5))
(check-error (checked-sum ex5))
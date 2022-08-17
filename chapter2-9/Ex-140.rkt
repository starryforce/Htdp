#lang htdp/bsl

; A List-of-Booleans is one of:
; - '()
; - (cons Boolean List-of-Booleans)

(define ex0 '())
(define ex1 (cons #true '()))
(define ex2 (cons #false '()))
(define ex3 (cons #true (cons #false '())))
(define ex4 (cons #true (cons #true '())))
(define ex5 (cons #false (cons #false '())))

; List-of-Booleans -> Boolean
; whether all of items in alob are #true
(define (all-true alob)
  (cond [(empty? alob) #true]
        [else (cond [(false? (first alob)) #false]
                    [else (all-true (rest alob))])]))

(check-expect (all-true ex0) #true)
(check-expect (all-true ex1) #true)
(check-expect (all-true ex2) #false)
(check-expect (all-true ex3) #false)
(check-expect (all-true ex4) #true)
(check-expect (all-true ex5) #false)


; List-of-Booleans -> Boolean
; whether at least one item of alob is true
(define (one-true alob)
  (cond [(empty? alob) #false]
        [else (cond [(not (false? (first alob))) #true]
                    [else (one-true (rest alob))])]))

(check-expect (one-true ex0) #false)
(check-expect (one-true ex1) #true)
(check-expect (one-true ex2) #false)
(check-expect (one-true ex3) #true)
(check-expect (one-true ex4) #true)
(check-expect (one-true ex5) #false)
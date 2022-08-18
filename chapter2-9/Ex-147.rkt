#lang htdp/bsl

; A NEList-of-Booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-Boolean)
; interpretation non-empty list of Boolean values

(define ex1 (cons #true '()))
(define ex2 (cons #false '()))
(define ex3 (cons #true ex1))
(define ex4 (cons #false ex2))
(define ex5 (cons #true ex2))


; NEList-of-Booleans -> Boolean;
; determine if all items in nlob is #true;
(define (all-true nlob)
  (cond [(empty? (rest nlob)) (first nlob)]
        [else (if (false? (first nlob)) #false (all-true (rest nlob)))]))

(check-expect (all-true ex1) #true)
(check-expect (all-true ex2) #false)
(check-expect (all-true ex3) #true)
(check-expect (all-true ex4) #false)
(check-expect (all-true ex5) #false)

; NEList-of-Booleans -> Boolean;
; determines whether at least one item on nlob is #true.
(define (one-true nlob)
  (cond [(empty? (rest nlob)) (first nlob)]
        [else (if (false? (first nlob)) (one-true (rest nlob)) #true)]))

(check-expect (one-true ex1) #true)
(check-expect (one-true ex2) #false)
(check-expect (one-true ex3) #true)
(check-expect (one-true ex4) #false)
(check-expect (one-true ex5) #true)
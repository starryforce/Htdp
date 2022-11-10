#lang htdp/isl


(define ex1 (list 10 20 30 40))
(define ex2 (list 15 20 50 100))

; Lon -> Lon
; adds 1 to each item on l
(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (add1 (first l))
       (add1* (rest l)))]))

(check-expect (add1* ex1) (list 11 21 31 41))
(check-expect (add1* ex2) (list 16 21 51 101))

; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) 5)
       (plus5 (rest l)))]))

(check-expect (plus5 ex1) (list 15 25 35 45))
(check-expect (plus5 ex2) (list 20 25 55 105))


(define (batch-operate l operation)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (operation (first l))
       (batch-operate (rest l) operation))]))

(define (add5 n) (+ n 5))

(check-expect (add1*.v2 ex1) (list 11 21 31 41))
(check-expect (add1*.v2 ex2) (list 16 21 51 101))
(check-expect (plus5.v2 ex1) (list 15 25 35 45))
(check-expect (plus5.v2 ex2) (list 20 25 55 105))

(define (add1*.v2 l) (batch-operate l add1))
(define (plus5.v2 l) (batch-operate l add5))

(define (minus2 n) (- n 2))

(define (minus2.v2 l) (batch-operate l minus2))
(check-expect (minus2.v2 ex1) (list 8 18 28 38))
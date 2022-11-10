#lang htdp/bsl

; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

(check-expect
  (average (cons 1 (cons 2 (cons 3 '())))) 2)

; List-of-temperatures -> Number
; computes the average temperature 
(define (average alot)
  (/ (sum alot) (how-many alot)))
 
; List-of-temperatures -> Number 
; adds up the temperatures on the given list 
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

 
; List-of-temperatures -> Number 
; counts the temperatures on the given list 
(define (how-many alot) 0)

; when apply to an empty list, it will signal an error


(define (checked-average alot)
  (cond [(empty? alot) (error "input should not be empty list")]
        [else (average alot)]))
#lang htdp/bsl

; (cons 2 '())
; #true
; #true #false

(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

(define ex1 (cons 1 (cons 2 '())))
(define ex2 (cons 3 (cons 2 '())))
(define ex3 (cons 0 (cons 3 (cons 2 '()))))

; NEList-of-temperatures -> Boolean;
; determine if nlots are sorted in descending order
(define (sorted>? nlots)
  (cond [(empty? (rest nlots)) #true]
        [else (cond [(> (first nlots) (first (rest nlots)))
                     (sorted>? (rest nlots))]
                    [else #false])]))



(check-expect (sorted>? ex1) #false)
(check-expect (sorted>? ex2) #true)
(check-expect (sorted>? ex3) #false)

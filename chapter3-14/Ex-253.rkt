#lang htdp/isl

; [Number -> Boolean]
(define (self-odd? n) (= (remainder n 2) 1))
; [Boolean String -> Boolean]

; [Number Number Number -> Number]
(define (self add a b c) (+ a b c))
; [Number -> [List-of Number]]
; [[List-of Number] -> Boolean]
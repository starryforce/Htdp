#lang htdp/bsl

; a def is like (define (variable variable variable ...) expr)

; 1. (define (f x) x)
; x is variable, x is also a expr

; 2. (define (f x) y)
; x is variable, y is a variable, also a expr

; 3. (define (f x y) 3)
; x and y are both variable, 3 is a value, also a expr
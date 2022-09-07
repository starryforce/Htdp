#lang htdp/bsl


; 1. (define (f "x") x)
; "x" is a value, not a variable

; 2. (define (f x y z) (x))
; x y z are both variable
; (x) is not a legal function application,so it is not a expr
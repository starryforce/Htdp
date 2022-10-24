#lang htdp/isl

; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
(define (argmax f lx) ...)

(define (i-square x) (* x x))

(argmax i-square (list 1 2 3 4 5 6))

; [X] [X -> Number] [NEList-of X] -> X
; finds the (first) item in lx that minimizes f
; if (argmin f (list x-1 ... x-n)) == x-i,
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2)), ...
(define (argmin f lx) ...)
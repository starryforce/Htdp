#lang htdp/bsl

; 1. (3 + 4)
; 3 is nor a primitive or a variable or cond

; 2. number?
; a primitive function call is like (primitive expr ...)
; but this is a single primitive function name

; 3. (x)
; x is a variable, it partially matches the shape of function application
; but it doesn't have a expr 
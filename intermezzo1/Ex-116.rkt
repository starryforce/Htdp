#lang htdp/bsl

; 1. x
; it's a variable, and every variable is an expression

; 2. (= x z)
; = is a primitive operator, x & z are both variables,
; so it's a primitive application

; 3. (= (= y z) 0)
; from the previos ,we know (= y z) is legal expr,
; then = is a primitive operator, (= y z) is an expr, 0 is a literal constant value
; so it's a primitive application too.


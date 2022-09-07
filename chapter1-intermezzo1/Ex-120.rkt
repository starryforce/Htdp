#lang htdp/bsl

; 1. (x)
; illegal

; 2. (+ 1 (not x))
; illegal
; + primitive's argurement should both be number, but (not x) is a Boolean


; 3. (+ 1 2 3)
; legal
; + is primitive and 1 2 3 are both value, aka expr
; it's an expr



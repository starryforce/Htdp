#lang htdp/isl+

; An S-expr is one of: 
; – Number
; – String
; – Symbol 
; – SL
 
; An SL is [List-of S-expr]


; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(define (count sexp sy) 0)
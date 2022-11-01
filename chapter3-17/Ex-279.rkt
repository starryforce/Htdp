#lang htdp/isl+

; valid, x could be a function
(lambda (x y) (x y y))


; not valid, lambda need a parameter
; (lambda () 10)

; valid
(lambda (x) x)

; valid
(lambda (x y) x)

; not valid, lack of the parentheses
; (lambda x 10)
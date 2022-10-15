#lang htdp/bsl+


; FSM-State is a Color.

; FSM-State FSM-State -> Boolean
; check if s1 s2 are same
(define (state=? s1 s2) (string=? s1 s2))

(check-expect (state=? "red" "red") #true)
(check-expect (state=? "red" "green") #false)
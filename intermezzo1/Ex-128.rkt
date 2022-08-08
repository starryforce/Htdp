#lang htdp/bsl

; "green" is not one of "red" "yellow" "grey"
(check-member-of "green" "red" "yellow" "grey")
; beyond 0.01
(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2)  0.01)
; #i0.9 is beyond range of #i0.6 - #i0.8
(check-range #i0.9 #i0.6 #i0.8)
; random not the same order
(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3)))
; 4 is an even
(check-satisfied 4 odd?)
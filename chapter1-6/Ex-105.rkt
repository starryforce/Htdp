#lang htdp/bsl


; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

-1
-4

4
5

(make-posn 1 2)
(make-posn 4 5)
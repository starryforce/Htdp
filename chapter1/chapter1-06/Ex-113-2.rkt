#lang htdp/bsl

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

; Any -> Boolean;
; is v an element of the Coordinate collection
(define (coordinate? v) (cond [(and (number? v) (or (> v 0) (< v 0)))  #true]
                              [(posn? v) #true]
                              [else #false]))

(check-expect (coordinate? 1) #true)
(check-expect (coordinate? -1) #true)
(check-expect (coordinate? 0) #false)
(check-expect (coordinate? "hello") #false)
(check-expect (coordinate? false) #false)
(check-expect (coordinate? (make-posn 2 4)) #true)
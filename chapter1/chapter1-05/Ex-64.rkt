#lang htdp/bsl

; posn -> number;
; computes the Manhattan  distance of ap to the origin
(define (manhattan-distance ap)
  (+ (posn-x ap) (posn-y ap)))

(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn 0 4)) 4)
(check-expect (manhattan-distance (make-posn 3 0)) 3)


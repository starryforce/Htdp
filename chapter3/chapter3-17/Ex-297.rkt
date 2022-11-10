#lang htdp/isl+

; Number Number Posn -> Number
; computes the distance between the points (x, y) and p.
(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))

(check-expect (distance-between 0 0 (make-posn 3 4)) 5)
(check-expect (distance-between 1 1 (make-posn 4 5)) 5)
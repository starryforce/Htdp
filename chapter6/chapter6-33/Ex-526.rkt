#lang htdp/isl+

(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels 
 
; Number -> Posn
; determines the point on the circle with CENTER 
; and RADIUS whose angle is 
 
; examples
; what are the x and y coordinates of the desired 
; point, when given: 120/360, 240/360, 360/360
(check-expect (circle-pt 0) (make-posn 400 200))
(check-expect (circle-pt 90) (make-posn 200 400))
(check-expect (circle-pt 180) (make-posn 0 200))
(check-expect (circle-pt 270) (make-posn 200 0))
(define (circle-pt factor)
  (end-point (posn-x CENTER) (posn-y CENTER) RADIUS factor))

; N N N N -> Posn
; calc the end point
(define (end-point start-x start-y r a)
  (local ((define (normalize x) (inexact->exact (round x))))
    (make-posn (normalize (+ start-x (* r (cos (* pi (/ a 180))))))
               (normalize (+ start-y (* r (sin (* pi (/ a 180)))))))))
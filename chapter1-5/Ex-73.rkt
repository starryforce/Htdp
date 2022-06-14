#lang htdp/bsl

; Posn -> Posn
; increases the x-coordinate of p by 3
(check-expect (x+ (make-posn 10 0)) (make-posn 13 0))
(define (x+ p)
  (posn-up-x p (+ (posn-x p) 3)))


; Posn Number -> Posn
; set x-coordinate of p to n
(define (posn-up-x p n)
  (make-posn n (posn-y p)))

(check-expect (posn-up-x (make-posn 12 30) 20) (make-posn 20 30))
(check-expect (posn-up-x (make-posn 34 53) 67) (make-posn 67 53))
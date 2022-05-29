#lang htdp/bsl

 (require 2htdp/image)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

; car roof
(define ROOF (rectangle (* 6 WHEEL-RADIUS) WHEEL-RADIUS "solid" "red"))
; car body
(define BODY (rectangle (+ WHEEL-DISTANCE (* 2 WHEEL-RADIUS)) (* 2 WHEEL-RADIUS) "solid" "red"))
; car main
(define MAIN (above ROOF BODY))

; car wheels
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle (* 1.5 WHEEL-RADIUS) WHEEL-RADIUS (* 2 WHEEL-RADIUS) "transparent"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR (underlay/align/offset "middle" "bottom" MAIN  0 WHEEL-RADIUS BOTH-WHEELS))
CAR
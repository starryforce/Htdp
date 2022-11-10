#lang htdp/bsl

; a ball with velocity is 3
(define SPEED 3)
; a ball is location far from top,
; and travells in direction
(define-struct balld [location direction])
(make-balld 10 "up")

(make-balld 30 "down")



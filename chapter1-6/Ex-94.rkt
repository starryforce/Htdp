#lang htdp/bsl

(require 2htdp/image)

(define TANK-WIDTH 40)
(define TANK-HEIGHT 20)
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "brown"))

(define MISSILE-SIDE (/ TANK-WIDTH 2))
(define MISSILE (triangle MISSILE-SIDE "solid" "red"))

(define UFO (overlay (circle 10 "solid" "green")
                     (ellipse 60 30 "solid" "grey")))


(define BACKGROUND-WIDTH 400)
(define BACKGROUND-HEIGHT 800)
(define BACKGROUND (rectangle BACKGROUND-WIDTH BACKGROUND-HEIGHT "solid" "royalblue"))
BACKGROUND
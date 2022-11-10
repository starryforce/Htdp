#lang htdp/bsl

(require 2htdp/image)

(define TANK-WIDTH 40)
(define TANK-HEIGHT 20)
(define TANK-SPEED 3)
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "brown"))

(define UFO-SPEED 5)
(define UFO (overlay (circle 10 "solid" "green")
                     (ellipse 60 10 "solid" "grey")))

(define MISSILE-SPEED (* 2 UFO-SPEED))
(define MISSILE-SIDE (/ TANK-WIDTH 2))
(define MISSILE (triangle MISSILE-SIDE "solid" "red"))

(define BACKGROUND-WIDTH 400)
(define BACKGROUND-HEIGHT 800)
(define BACKGROUND (rectangle BACKGROUND-WIDTH BACKGROUND-HEIGHT "solid" "royalblue"))

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s 
(define (missile-render.v2 m s)
  s)

(check-expect (missile-render.v2 #false BACKGROUND) BACKGROUND)
(check-expect (missile-render.v2 (make-posn 32 (- BACKGROUND-HEIGHT TANK-HEIGHT 10))
                                 BACKGROUND)
              (place-image MISSILE 32 (- BACKGROUND-HEIGHT TANK-HEIGHT 10) BACKGROUND))
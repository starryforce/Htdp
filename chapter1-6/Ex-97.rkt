#lang htdp/bsl

(require 2htdp/image)

(define TANK-WIDTH 40)
(define TANK-HEIGHT 20)
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "brown"))

(define MISSILE-SIDE (/ TANK-WIDTH 2))
(define MISSILE (triangle MISSILE-SIDE "solid" "red"))

(define UFO (overlay (circle 10 "solid" "green")
                     (ellipse 60 10 "solid" "grey")))


(define BACKGROUND-WIDTH 400)
(define BACKGROUND-HEIGHT 800)
(define BACKGROUND (rectangle BACKGROUND-WIDTH BACKGROUND-HEIGHT "solid" "royalblue"))

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; Tank Image -> Image 
; adds t to the given image im
(define (tank-render t im) (place-image TANK
                                        (tank-loc t)
                                        (image-height im)
                                        im))
(check-expect (tank-render (make-tank 30 3) BACKGROUND)
              (place-image TANK 30 BACKGROUND-HEIGHT BACKGROUND))
 
; UFO Image -> Image 
; adds u to the given image im
(define (ufo-render u im) (place-image UFO
                                       (posn-x u)
                                       (posn-y u)
                                       im))
(check-expect (ufo-render (make-posn 100 150) BACKGROUND)
              (place-image UFO 100 150 BACKGROUND))

; Missile Image -> Image 
; adds m to the given image im
(define (missile-render u im) (place-image MISSILE
                                       (posn-x u)
                                       (posn-y u)
                                       im))
(check-expect (missile-render (make-posn 100 150) BACKGROUND)
              (place-image MISSILE 100 150 BACKGROUND))

; when tank and ufo not cover each other.
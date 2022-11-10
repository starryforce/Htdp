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

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

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

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game


(define ex1 (make-aim (make-posn 20 10) (make-tank 28 (- TANK-SPEED))))
(define ex2 (make-fired (make-posn 20 10)
                        (make-tank 28 (- TANK-SPEED))
                        (make-posn 28 (- BACKGROUND-HEIGHT TANK-HEIGHT))))
(define ex3 (make-fired (make-posn 20 100)
                        (make-tank 100 TANK-SPEED)
                        (make-posn 22 103)))

; SIGS -> SIGS
; generate next state from current si
(define (si-move w)
  (si-move-proper w (jump 50)))

; SIGS Number -> SIGS 
; moves the space-invader objects predictably by delta
(define (si-move-proper w delta) (cond [(aim? w) (make-aim (tick-ufo (aim-ufo w) delta)
                                                           (tick-tank (aim-tank w)))]
                                       [(fired? w) (make-fired (tick-ufo (fired-ufo w) delta)
                                                               (tick-tank (fired-tank w))
                                                               (tick-missile (fired-missile w)))]))

(check-expect (si-move-proper ex1 3)
              (make-aim (make-posn (+ 20 3) (+ 10 UFO-SPEED))
                        (make-tank (- 28 TANK-SPEED) (- TANK-SPEED))))
(check-expect (si-move-proper ex2 -4)
              (make-fired (make-posn (+ 20 -4) (+ 10 UFO-SPEED))
                          (make-tank (- 28 TANK-SPEED) (- TANK-SPEED))
                          (make-posn 28 (- BACKGROUND-HEIGHT TANK-HEIGHT MISSILE-SPEED))))
(check-expect (si-move-proper ex3 0)
              (make-fired (make-posn 20 (+ 100 UFO-SPEED))
                          (make-tank (+ 100 TANK-SPEED) TANK-SPEED)
                          (make-posn 22 (- 103 MISSILE-SPEED))))

; UFO Number -> UFO
; UFO falls down UFO-SPEED pixels every tick
; move delta pixels in horizion direction
(define (tick-ufo u delta) (make-posn (cond [(< (+ (posn-x u) delta) 0) 0]
                                            [(> (+ (posn-x u) delta) BACKGROUND-WIDTH) BACKGROUND-WIDTH]
                                            [else (+ (posn-x u) delta)])
                                      (cond [(> (+ (posn-y u) UFO-SPEED) BACKGROUND-HEIGHT) BACKGROUND-HEIGHT]
                                            [else (+ (posn-y u) UFO-SPEED)])))

(check-expect (tick-ufo (make-posn 20 100) 10)
              (make-posn (+ 20 10) (+ 100 UFO-SPEED)))
(check-expect (tick-ufo (make-posn 5 200) -10)
              (make-posn 0 (+ 200 UFO-SPEED)))
(check-expect (tick-ufo (make-posn 395 799) 10)
              (make-posn BACKGROUND-WIDTH BACKGROUND-HEIGHT))

; TANK -> TANK
; TANK moves (tank-vel t) pixels from (tank-loc t) per tick
(define (tick-tank t) (make-tank (cond [(< (+ (tank-loc t) (tank-vel t)) 0) 0]
                                       [(> (+ (tank-loc t) (tank-vel t)) BACKGROUND-WIDTH) BACKGROUND-WIDTH]
                                       [else (+ (tank-loc t) (tank-vel t))])
                                 (tank-vel t)))

(check-expect (tick-tank (make-tank 20 5))
              (make-tank 25 5))
(check-expect (tick-tank (make-tank 1 -5))
              (make-tank 0 -5))
(check-expect (tick-tank (make-tank BACKGROUND-WIDTH 5))
              (make-tank BACKGROUND-WIDTH 5))

; MISSILE -> MISSILE
; MISSILE move up MISSILE-SPEED pixels per tick
(define (tick-missile m) (make-posn (posn-x m)
                                     (- (posn-y m) MISSILE-SPEED)))

(check-expect (tick-missile (make-posn 20 40))
              (make-posn 20 (- 40 MISSILE-SPEED)))

; Number -> Number
(define (jump range) (- (random range) (/ range 2)))
(check-random (jump 50) (- (random 50) 25))
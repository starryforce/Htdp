#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 800)

(define TANK (above (wedge 10 180 "solid" "grey") (rectangle 40 10 "solid" "grey")))
(define MOON (circle 20 "solid" "lightgoldenrodyellow"))
(define SCENE (place-image/align MOON 80 40 "right" "top" (empty-scene SCENE-WIDTH SCENE-HEIGHT "dark blue")))

(define MISSILE (isosceles-triangle 20 30 "solid" "red"))


; A Missile is a Posn

; A Direction is one of:
; - "left"
; - "right"

(define-struct tank [position direction])
; A Tank is a structure:
; (make-tank Number Direction Posn)
; interpretation (make-tank p d) represents a tank
; current at position p moving towards d

(define tank1 (make-tank 0 "left"))
(define tank2 (make-tank 0 "right"))
(define tank3 (make-tank (/ SCENE-WIDTH 4) "left"))
(define tank4 (make-tank (/ SCENE-WIDTH 4) "right"))
(define tank5 (make-tank SCENE-WIDTH "left"))
(define tank6 (make-tank SCENE-WIDTH "right"))

; A Missile is a Posn
; A List-of-missiles is one of:
; - '()
; (cons Missile List-of-missiles)

(define-struct game [tank missiles])
; A Game is a structure:
; (make-game Tank List-of-missiles)
; interpretation (make-game t alom) represents
; a tank with some missiles alom


; Tank -> Image
; render the tank t onto the background
(define (render t) (place-image/align TANK
                                          (tank-position t)
                                          SCENE-HEIGHT
                                          "middle"
                                          "bottom"
                                          SCENE))

(check-expect (render tank1) (place-image/align TANK
                                          0
                                          SCENE-HEIGHT
                                          "middle"
                                          "bottom"
                                          SCENE))
(check-expect (render tank3) (place-image/align TANK
                                          100
                                          SCENE-HEIGHT
                                          "middle"
                                          "bottom"
                                          SCENE))

; Tank -> Tank
; Tank moves every tick
(define (tock t)
  (make-tank (cond [(string=? (tank-direction t) "left") (max (sub1 (tank-position t)) 0)]
                   [(string=? (tank-direction t) "right") (min (add1 (tank-position t)) SCENE-WIDTH)])
             (tank-direction t)))

(check-expect (tock tank1) tank1)
(check-expect (tock tank2) (make-tank 1 "right"))
(check-expect (tock tank3) (make-tank (sub1 (/ SCENE-WIDTH 4)) "left"))
(check-expect (tock tank4) (make-tank (add1 (/ SCENE-WIDTH 4)) "right"))
(check-expect (tock tank5) (make-tank (sub1 SCENE-WIDTH) "left"))
(check-expect (tock tank6) tank6)

; Tank KeyEvent -> Tank
; press "left" to make tank move to left,
; press "right" to make tank move to right,
(define (control t ke)
  (make-tank (tank-position t) (cond [(string=? ke "left") "left"]
                                     [(string=? ke "right") "right"]
                                     [else (tank-direction t)]
                                     )))

(check-expect (control tank3 "left") (make-tank (/ SCENE-WIDTH 4) "left"))
(check-expect (control tank3 "right") (make-tank (/ SCENE-WIDTH 4) "right"))

; Number -> Tank
; start game with tank at x-coordinate of initial
(define (game-main initial)
  (big-bang (make-tank initial "right")
    [on-draw render]
    [on-key control]
    [on-tick tock]))
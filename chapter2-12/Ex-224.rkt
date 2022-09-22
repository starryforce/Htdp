#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 800)

(define TANK (above (wedge 10 180 "solid" "grey") (rectangle 40 10 "solid" "grey")))
(define MOON (circle 20 "solid" "lightgoldenrodyellow"))
(define SCENE (place-image/align MOON 80 40 "right" "top" (empty-scene SCENE-WIDTH SCENE-HEIGHT "dark blue")))


; A Direction is one of:
; - "left"
; - "right"

(define-struct tank [position direction])
; A Tank is a structure:
; (make-tank Number Direction)
; interpretation (make-tank p d) represents a tank
; current at position p moving towards d

(define tank1 (make-tank 0 "left"))
(define tank2 (make-tank 0 "right"))
(define tank3 (make-tank (/ SCENE-WIDTH 4) "left"))
(define tank4 (make-tank (/ SCENE-WIDTH 4) "right"))
(define tank5 (make-tank SCENE-WIDTH "left"))
(define tank6 (make-tank SCENE-WIDTH "right"))

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
(define (tock t) t)

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
    [on-key control]))
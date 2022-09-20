#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 800)

(define TANK (above (wedge 10 180 "solid" "grey") (rectangle 40 10 "solid" "grey")))
(define TANK-SPEED 10)
(define MOON (circle 20 "solid" "lightgoldenrodyellow"))
(define SCENE (place-image/align MOON 80 40 "right" "top" (empty-scene SCENE-WIDTH SCENE-HEIGHT "dark blue")))


; A Tank is a number
; represent the x-coordinate of the tank

(define tank1 0)
(define tank2 200)

; Tank -> Image
; render the tank t onto the background
(define (render t) (place-image/align TANK
                                          t
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
(check-expect (render tank2) (place-image/align TANK
                                          200
                                          SCENE-HEIGHT
                                          "middle"
                                          "bottom"
                                          SCENE))

; Tank -> Tank
; Tank moves according it's current direction
(define (tock t) t)



; Tank KeyEvent -> Tank
; press "left" to make tank move to left,
; press "right" to make tank move to right,
(define (control t ke)
  (cond [(string=? ke "left") (max (- t TANK-SPEED) 0) ]
        [(string=? ke "right") (min (+ t TANK-SPEED) SCENE-WIDTH)]
        [else t]))

(check-expect (control 0 "left") 0)
(check-expect (control SCENE-WIDTH "right") SCENE-WIDTH)
(check-expect (control 100 "left") (- 100 TANK-SPEED))
(check-expect (control 100 "right") (+ 100 TANK-SPEED))
(check-expect (control 100 "a") 100)


; Number -> Tank
; start game with tank at x-coordinate of initial
(define (game-main initial)
  (big-bang initial
    [on-draw render]
    [on-key control]))
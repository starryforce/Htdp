#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 800)

(define TANK (above (wedge 10 180 "solid" "grey") (rectangle 40 10 "solid" "grey")))
(define TANK-SPEED 2)
(define MOON (circle 20 "solid" "lightgoldenrodyellow"))
(define SCENE (place-image/align MOON 80 40 "right" "top" (empty-scene SCENE-WIDTH SCENE-HEIGHT "dark blue")))

(define MISSILE-SPEED 3)
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
(define missile1 (make-posn 10 20))
(define missile2 (make-posn 40 50))
(define missile3 (make-posn 20 -8))
; A List-of-missiles is one of:
; - '()
; (cons Missile List-of-missiles)
(define lom0 '())
(define lom1 (list missile1 missile2))
(define lom2 (list missile1 missile3))

(define-struct game [tank missiles])
; A Game is a structure:
; (make-game Tank List-of-missiles)
; interpretation (make-game t alom) represents
; a tank with some missiles alom
(define game1 (make-game tank3 lom1))
(define game2 (make-game tank3 lom2))

; Game -> Image
; render current game state
(define (render g)
  (render-tank (game-tank g)
               (render-missiles (game-missiles g)
                                SCENE)))

(check-expect (render game1) (render-tank tank3 (render-missiles lom1 SCENE)))

; Tank -> Image
; render the tank t onto the background
(define (render-tank t s) (place-image/align TANK
                                          (tank-position t)
                                          SCENE-HEIGHT
                                          "middle"
                                          "bottom"
                                          s))

(check-expect (render-tank tank1 SCENE) (place-image/align TANK
                                          0
                                          SCENE-HEIGHT
                                          "middle"
                                          "bottom"
                                          SCENE))
(check-expect (render-tank tank3 SCENE) (place-image/align TANK
                                          100
                                          SCENE-HEIGHT
                                          "middle"
                                          "bottom"
                                          SCENE))

; List-of-missiles -> Image
; put a list of missiles onto the background
(define (render-missiles alom s)
  (cond [(empty? alom) s]
        [else (place-image MISSILE
                           (posn-x (first alom))
                           (posn-y (first alom))
                           (render-missiles (rest alom) s))]))

(check-expect (render-missiles lom0 SCENE) SCENE)
(check-expect (render-missiles lom1 SCENE) (place-image MISSILE
                                         40
                                         50
                                         (place-image MISSILE
                                         10
                                         20
                                         SCENE)))

; Tank -> Tank
; Tank moves every tick
(define (tock-tank t)
  (make-tank (cond [(string=? (tank-direction t) "left") (max (sub1 (tank-position t)) 0)]
                   [(string=? (tank-direction t) "right") (min (add1 (tank-position t)) SCENE-WIDTH)])
             (tank-direction t)))

(check-expect (tock-tank tank1) tank1)
(check-expect (tock-tank tank2) (make-tank 1 "right"))
(check-expect (tock-tank tank3) (make-tank (sub1 (/ SCENE-WIDTH 4)) "left"))
(check-expect (tock-tank tank4) (make-tank (add1 (/ SCENE-WIDTH 4)) "right"))
(check-expect (tock-tank tank5) (make-tank (sub1 SCENE-WIDTH) "left"))
(check-expect (tock-tank tank6) tank6)


; List-of-missiles -> List-of-missiles
; Missiles move upward every tick
(define (tock-missiles alom)
  (cond [(empty? alom) '()]
        [else (cons (make-posn (posn-x (first alom)) (- (posn-y (first alom)) 3))
                    (tock-missiles (rest alom)))]))

(check-expect (tock-missiles lom0) lom0)
(check-expect (tock-missiles lom1) (list (make-posn 10 (- 20 MISSILE-SPEED))
                                         (make-posn 40 (- 50 MISSILE-SPEED))))

; List-of-missiles -> List-of-missiles
; clear missiles out of range, say missile's y coordinate is less than zero
(define (clear-missiles alom)
  (cond [(empty? alom) '()]
        [else (cond [(> (posn-y (first alom)) 0)
                     (cons (first alom) (clear-missiles (rest alom)))]
                    [else (clear-missiles (rest alom))])]))

(check-expect (clear-missiles '()) '())
(check-expect (clear-missiles lom1) lom1)
(check-expect (clear-missiles lom2) (list missile1))

; Game -> Game
; move every missiles and tank for every clock tick
(define (tock g)
  (make-game (tock-tank (game-tank g))
             (clear-missiles (tock-missiles (game-missiles g)))))

(check-expect (tock game1)
              (make-game (tock-tank tank3)
                         (tock-missiles lom1)))

(check-expect (tock game2)
              (make-game (tock-tank tank3)
                         (clear-missiles (tock-missiles lom2))))

; Tank KeyEvent -> Tank
; press "left" to make tank move to left,
; press "right" to make tank move to right,
(define (control-tank t ke)
  (make-tank (tank-position t) (cond [(string=? ke "left") "left"]
                                     [(string=? ke "right") "right"]
                                     [else (tank-direction t)]
                                     )))

(check-expect (control-tank tank3 "left") (make-tank (/ SCENE-WIDTH 4) "left"))
(check-expect (control-tank tank3 "right") (make-tank (/ SCENE-WIDTH 4) "right"))


; Game KeyEvent -> List-of-missiles
; press space to lanch a missile from current position of tank
(define (fire g ke)
  (cond 
        [(string=? " " ke) (cons (make-posn (tank-position (game-tank g)) SCENE-HEIGHT)
                    (game-missiles g))]
        [else (game-missiles g)]))

(check-expect (fire game1 "right") (game-missiles game1))
(check-expect (fire game1 "k") (game-missiles game1))
(check-expect (fire game1 " ") (cons (make-posn (/ SCENE-WIDTH 4) SCENE-HEIGHT) lom1))

; Game KeyEvent -> Game
; press "left" or "right" arrow key to move the tank
; press "space" to lanch a missile
(define (control g ke)
  (make-game (control-tank (game-tank g) ke)
             (fire g ke)))


(check-expect (control game1 "left") (make-game (control-tank (game-tank game1) "left") (game-missiles game1)))
(check-expect (control game1 " ") (make-game (game-tank game1)
                                             (cons (make-posn (/ SCENE-WIDTH 4) SCENE-HEIGHT)
                                                   (game-missiles game1))))

; Number -> Tank
; start game with tank at x-coordinate of initial
(define (game-main speed)
  (big-bang (make-game (make-tank 0 "right") '())
    [on-draw render]
    [on-key control]
    [on-tick tock speed]))
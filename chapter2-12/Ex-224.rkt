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

(define UFO-SPEED 1)
(define UFO (overlay/offset (ellipse 35 25 "solid" "pink") 0 10 (ellipse 60 30 "solid" "silver")))

(define BOMB-SPEED (* 2 UFO-SPEED))
(define BOMB (circle 8 "solid" "red"))


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
; - (cons Missile List-of-missiles)
(define lom0 '())
(define lom1 (list missile1 missile2))
(define lom2 (list missile1 missile3))

; A Ufo is a Posn
(define ufo1 (make-posn 50 50))
(define ufo2 (make-posn 70 90))

; A List-of-ufos is one of:
; - '()
; - (cons UFO List-of-ufos)
(define fleet0 '())
(define fleet1 (list ufo1))
(define fleet2 (list ufo1 ufo2))


(define-struct game [tank missiles fleet])
; A Game is a structure:
; (make-game Tank List-of-missiles List-of-ufos)
; interpretation (make-game t alom alou) represents
; a tank with some missiles alom, some landing ufo alou
(define game1 (make-game tank3 lom1 fleet1))
(define game2 (make-game tank3 lom2 fleet2))

; Game -> Image
; render current game state
(define (render g)
  (render-tank (game-tank g)
               (render-missiles (game-missiles g)
                                (render-ufos (game-fleet g) SCENE))))

(check-expect (render game1) (render-tank tank3 (render-missiles lom1 (render-ufos fleet1 SCENE))))

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

; List-of-ufos -> Image
; put a list of ufos onto the background
(define (render-ufos alou s)
  (cond [(empty? alou) s]
        [else (place-image UFO
                           (posn-x (first alou))
                           (posn-y (first alou))
                           (render-ufos (rest alou) s))]))

(check-expect (render-ufos fleet0 SCENE) SCENE)
(check-expect (render-ufos fleet2 SCENE) (place-image UFO
                                         70
                                         90
                                         (place-image UFO
                                         50
                                         50
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

; List-of-ufos -> List-of-ufos
; Ufos move downward every tick
(define (tock-ufos alou)
  (cond [(empty? alou) '()]
        [else (cons (make-posn  (posn-x (first alou))
                                (+ (posn-y (first alou)) UFO-SPEED))
                    (tock-ufos (rest alou)))]))

(check-expect (tock-ufos fleet0) fleet0)
(check-expect (tock-ufos fleet1) (list (make-posn 50 (+ 50 UFO-SPEED))))
(check-expect (tock-ufos fleet2) (list (make-posn 50 51) (make-posn 70 91)))


; List-of-ufos -> List-of-ufos
; when ufo is less than 5, generate a ufo at random place on top
(define (join-ufo alou) alou)


; Game -> Game
; move every missiles and tank for every clock tick
(define (tock g)
  (make-game (tock-tank (game-tank g))
             (clear-missiles (tock-missiles (game-missiles g)))
             (tock-ufos (game-fleet g))))



(check-expect (tock game1)
              (make-game (tock-tank tank3)
                         (tock-missiles lom1)
                         (tock-ufos fleet1)))

(check-expect (tock game2)
              (make-game (tock-tank tank3)
                         (clear-missiles (tock-missiles lom2))
                         (tock-ufos fleet2)))


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
             (fire g ke)
             (game-fleet g)))


(check-expect (control game1 "left") (make-game (control-tank (game-tank game1) "left") (game-missiles game1) (game-fleet game1)))
(check-expect (control game1 " ") (make-game (game-tank game1)
                                             (cons (make-posn (/ SCENE-WIDTH 4) SCENE-HEIGHT)
                                                   (game-missiles game1))
                                             (game-fleet game1)))

; Number -> Tank
; start game with tank at x-coordinate of initial
(define (game-main speed)
  (big-bang (make-game (make-tank 0 "right") '() '())
    [on-draw render]
    [on-key control]
    [on-tick tock speed]))

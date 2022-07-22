#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

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

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

(define-struct sigs [ufo tank missile])

(define ex1 (make-sigs (make-posn 20 10) (make-tank 28 (- TANK-SPEED)) #false))
(define ex2 (make-sigs (make-posn 20 10)
                        (make-tank 28 (- TANK-SPEED))
                        (make-posn 28 (- BACKGROUND-HEIGHT TANK-HEIGHT))))
(define ex3 (make-sigs (make-posn 20 100)
                        (make-tank 100 TANK-SPEED)
                        (make-posn 22 103)))

; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
 
; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation #false means the missile is in the tank;
; Posn says the missile is at that location

; SIGS.v2 -> Image 
; renders the given game state on top of BACKGROUND 
(define (si-render.v2 s)
  (tank-render
    (sigs-tank s)
    (ufo-render (sigs-ufo s)
                (missile-render.v2 (sigs-missile s)
                                   BACKGROUND))))

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

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s 
(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) s)]))


(check-expect (missile-render.v2 #false BACKGROUND) BACKGROUND)
(check-expect (missile-render.v2 (make-posn 32 (- BACKGROUND-HEIGHT TANK-HEIGHT 10))
                                 BACKGROUND)
              (place-image MISSILE 32 (- BACKGROUND-HEIGHT TANK-HEIGHT 10) BACKGROUND))



; SIGS.v2 -> SIGS.v2
; generate next state from current si
(define (si-move w)
  (si-move-proper w (jump 6)))

; SIGS.v2 Number -> SIGS.v2
; moves the space-invader objects predictably by delta
(define (si-move-proper w delta) (make-sigs (tick-ufo (sigs-ufo w) delta)
                                             (tick-tank (sigs-tank w))
                                             (tick-missile (sigs-missile w))))

(check-expect (si-move-proper ex1 3)
              (make-sigs (make-posn (+ 20 3) (+ 10 UFO-SPEED))
                        (make-tank (- 28 TANK-SPEED) (- TANK-SPEED))
                        #false))
(check-expect (si-move-proper ex2 -4)
              (make-sigs (make-posn (+ 20 -4) (+ 10 UFO-SPEED))
                          (make-tank (- 28 TANK-SPEED) (- TANK-SPEED))
                          (make-posn 28 (- BACKGROUND-HEIGHT TANK-HEIGHT MISSILE-SPEED))))
(check-expect (si-move-proper ex3 0)
              (make-sigs (make-posn 20 (+ 100 UFO-SPEED))
                          (make-tank (+ 100 TANK-SPEED) TANK-SPEED)
                          (make-posn 22 (- 103 MISSILE-SPEED))))

; Number -> Number
(define (jump range) (- (random range) (/ range 2)))
(check-random (jump 6) (- (random 6) 3))


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
(define (tick-missile m) (if (posn? m) (make-posn (posn-x m)
                                     (- (posn-y m) MISSILE-SPEED)) #false))

(check-expect (tick-missile (make-posn 20 40))
              (make-posn 20 (- 40 MISSILE-SPEED)))
(check-expect (tick-missile #false) #false)

; SIGS.v2 KeyEvent -> SIGS.v2
; KeyEvent is one of the following
; - "left"
; - "right"
; - " "
(define (si-control si ke) (cond [(string=? " " ke) (cond [(posn? (sigs-missile si)) si]
                                                          [else (make-sigs (sigs-ufo si) (sigs-tank si) (make-posn
                                                                          (tank-loc (sigs-tank si))
                                                                          (- BACKGROUND-HEIGHT TANK-HEIGHT)))])]
                                 [else (make-sigs (sigs-ufo si)
                                                           (control-tank (sigs-tank si) ke)
                                                           (sigs-missile si))]))

(check-expect (si-control ex1 "left") ex1)
(check-expect (si-control ex1 "right")
              (make-sigs (make-posn 20 10) (make-tank 28 TANK-SPEED) #false))
(check-expect (si-control ex1 " ")
              (make-sigs (make-posn 20 10)
                        (make-tank 28 (- TANK-SPEED))
                        (make-posn 28 (- BACKGROUND-HEIGHT TANK-HEIGHT))))

; TANK KeyEvent -> TANK
; change moving direction of the t
(define (control-tank t ke) (cond [(string=? "left" ke) (make-tank (tank-loc t) (- (abs (tank-vel t))))]
                                  [(string=? "right" ke) (make-tank (tank-loc t) (abs (tank-vel t)))]
                                  [else t]))
(define tank1 (make-tank 5 10))
(define tank2 (make-tank 5 -10))

(check-expect (control-tank tank1 "left") tank2)
(check-expect (control-tank tank1 "right") tank1)
(check-expect (control-tank tank2 "left") tank2)
(check-expect (control-tank tank2 "right") tank1)
(check-expect (control-tank tank2 "k") tank2)

; SIGS.v2 -> Boolean
; the game ends when
; - if the UFO lands -> human win
; - if the missile hits the UFO -> human loss
(define (si-game-over si) (or (is-loss (sigs-ufo si))
                              (is-win (sigs-ufo si) (sigs-missile si))))

(check-expect (si-game-over ex1) #false)
(check-expect (si-game-over ex2) #false)
(check-expect (si-game-over ex3) #true)

(define LIMIT 20)
; Posn Posn -> Boolean
; check is the distance between p1 & p2 less than limit
(define (close-enough p1 p2) (<= (sqrt (+
                                    (sqr (- (posn-x p1) (posn-x p2)))
                                    (sqr (- (posn-y p1) (posn-y p2))))) LIMIT))

; UFO -> Boolean
; when ufo land on the ground, we human loss
(define (is-loss u) (close-enough u (make-posn (posn-x u) BACKGROUND-HEIGHT)))

(check-expect (is-loss (make-posn 20 50)) #false)
(check-expect (is-loss (make-posn 20 (- BACKGROUND-HEIGHT LIMIT))) #true)
(check-expect (is-loss (make-posn 20 (- BACKGROUND-HEIGHT LIMIT 1))) #false)

; UFO MISSILE -> Boolean
; when m hit u (close enough) ,we human win
(define (is-win u m) (cond [(posn? m) (close-enough u m)]
                               [else #false]))

(check-expect (is-win (make-posn 20 50) #false) #false)
(check-expect (is-win (make-posn 20 50) (make-posn 20 30)) #true)



(define (si-render-final si) (if (is-win (sigs-ufo si) (sigs-missile si))
                                 (text "You save the Earth !!!" 32 "green")
                                 (text "Earth get fucked up!!!" 32 "red")))




(check-expect (close-enough (make-posn 10 20) (make-posn 12 24)) #true)
(check-expect (close-enough (make-posn 10 20) (make-posn 10 30)) #true)
(check-expect (close-enough (make-posn 10 20) (make-posn 20 70)) #false)



(define (si-main loc)
  (big-bang (make-sigs (make-posn (/ BACKGROUND-WIDTH 2) 0) (make-tank loc -5) #false) 
    [to-draw si-render.v2]
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over si-render-final]))

(si-main 5)

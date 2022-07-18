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
  (si-move-proper w (jump 6)))

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
(check-random (jump 6) (- (random 6) 3))

; SIGS KeyEvent -> SIGS
; KeyEvent is one of the following
; - "left"
; - "right"
; - " "
; SIGS is one of the following:
; - "aim"
; - "fired"
(define (si-control si ke) (cond [(and (string=? " " ke) (aim? si))
                                  (make-fired (aim-ufo si) (aim-tank si) (make-posn
                                                                          (tank-loc (aim-tank si))
                                                                          (- BACKGROUND-HEIGHT TANK-HEIGHT)))]
                                 [(aim? si) (make-aim (aim-ufo si) (control-tank (aim-tank si) ke))]
                                 [(fired? si) (make-fired (fired-ufo si)
                                                           (control-tank (fired-tank si) ke)
                                                           (fired-missile si))]
                                 [else si]))

(check-expect (si-control ex1 "left") ex1)
(check-expect (si-control ex1 "right")
              (make-aim (make-posn 20 10) (make-tank 28 TANK-SPEED)))
(check-expect (si-control ex1 " ")
              (make-fired (make-posn 20 10)
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


; SIGS -> Image
; renders the given game state on top of BACKGROUND 
; for examples see figure 32
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
       (fired-tank s)
       (ufo-render (fired-ufo s)
                   (missile-render (fired-missile s)
                                   BACKGROUND)))]))

; when tank and ufo not cover each other.

; SIGS -> Boolean
; the game ends when
; - if the UFO lands
; - if the missile hits the UFO
(define (si-game-over si) (cond [(aim? si) (close-enough (aim-ufo si)
                                                         (make-posn (posn-x (aim-ufo si)) BACKGROUND-HEIGHT))]
                                [(fired? si) (or (close-enough (fired-ufo si)
                                                               (make-posn (posn-x (fired-ufo si)) BACKGROUND-HEIGHT))
                                                 (close-enough (fired-ufo si)
                                                               (fired-missile si)))]))

(check-expect (si-game-over ex1) #false)
(check-expect (si-game-over ex2) #false)
(check-expect (si-game-over ex3) #true)

(define LIMIT 20)
; Posn Posn -> Boolean
; check is the distance between p1 & p2 less than limit
(define (close-enough p1 p2) (<= (sqrt (+
                                    (sqr (- (posn-x p1) (posn-x p2)))
                                    (sqr (- (posn-y p1) (posn-y p2))))) LIMIT))

(check-expect (close-enough (make-posn 10 20) (make-posn 12 24)) #true)
(check-expect (close-enough (make-posn 10 20) (make-posn 10 30)) #true)
(check-expect (close-enough (make-posn 10 20) (make-posn 20 70)) #false)
(check-expect (close-enough (make-posn 10 20) (make-posn 10 30)) #true)


(define (si-render-final si) (text "Earth get fucked up!!!" 16 "red"))


(define (si-main loc)
  (big-bang (make-aim (make-posn (/ BACKGROUND-WIDTH 2) 0) (make-tank loc -5)) 
    [to-draw si-render]
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over si-render-final]))

(si-main 5)

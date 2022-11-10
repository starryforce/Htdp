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


(define ex1 (make-aim (make-posn 20 10) (make-tank 28 -3)))
(define ex2 (make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- BACKGROUND-HEIGHT TANK-HEIGHT))))
(define ex3 (make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103)))


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

(define LIMIT 10)
; Posn Posn -> Boolean
; check is the distance between p1 & p2 less than limit
(define (close-enough p1 p2) (<= (sqrt (+
                                    (sqr (- (posn-x p1) (posn-x p2)))
                                    (sqr (- (posn-y p1) (posn-y p2))))) LIMIT))

(check-expect (close-enough (make-posn 10 20) (make-posn 12 24)) #true)
(check-expect (close-enough (make-posn 10 20) (make-posn 10 30)) #true)
(check-expect (close-enough (make-posn 10 20) (make-posn 20 30)) #false)
(check-expect (close-enough (make-posn 10 20) (make-posn 10 30)) #true)


(define (si-render-final si) (text "Earth get fucked up!!!" 16 "red"))
(si-render-final 2)


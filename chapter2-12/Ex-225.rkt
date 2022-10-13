#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define SCENE-WIDTH 20)
(define SCENE-HEIGHT 20)
(define GRID-SIZE 20)
(define SCENE (empty-scene (* SCENE-WIDTH GRID-SIZE) (* SCENE-HEIGHT GRID-SIZE) "green"))

; airplance's active range
(define MIN-X 0)
(define MIN-Y 0)
(define MAX-X (sub1 SCENE-WIDTH))
(define MAX-Y (sub1 SCENE-HEIGHT))

(define TREE (circle (/ GRID-SIZE 2) "solid" "green"))
(define FIRE (square GRID-SIZE "solid" "red"))
(define AIRPLANE (triangle GRID-SIZE "solid" "white"))

(define UP "up")
(define RIGHT "right")
(define DOWN "down")
(define LEFT "left")


; A Direction is one of:
; - UP
; - RIGHT
; - DOWN
; - LEFT

(define-struct airplane [position direction])
; An Airplane is a structure:
; (make-airplne Posn Direction)
; interpretation: (make-airplane p d) represents an airplane
; now at position p and flying towards d
(define airplane11 (make-airplane (make-posn 10 10) UP))
(define airplane12 (make-airplane (make-posn 10 10) RIGHT))
(define airplane13 (make-airplane (make-posn 10 10) DOWN))
(define airplane14 (make-airplane (make-posn 10 10) LEFT))
(define airplane21 (make-airplane (make-posn 10 MIN-Y) UP))
(define airplane22 (make-airplane (make-posn MAX-X 10) RIGHT))
(define airplane23 (make-airplane (make-posn 10 MAX-Y) DOWN))
(define airplane24 (make-airplane (make-posn MIN-X 10) LEFT))

; A Fire is a Posn:
(define fire1 (make-posn 5 10))
(define fire2 (make-posn 10 15))

; A List-of-fires is one of:
; - '()
; - (cons Fire List-of-fires)
; interpretation represents places that is on fire
(define lof0 '())
(define lof1 (list fire1))
(define lof2 (list fire1 fire2))


(define-struct game [airplane fires])
; A Game is a structure:
; (make-game Airplane List-of-fires)
; interpretation (make-game a alof) represents
; a place with a flying to fire-fighting with some fires
(define game1 (make-game airplane11 lof0))
(define game2 (make-game airplane11 lof1))
(define game3 (make-game airplane11 lof2))
(define game4 (make-game (make-airplane (make-posn 10 15) UP) lof2))

; Airplane Image -> Image
; put the airplane onto the background
(define (render-airplane a bg)
  (place-image/align (rotate (cond [(string=? UP (airplane-direction a)) 0]
                                   [(string=? RIGHT (airplane-direction a)) 270]
                                   [(string=? DOWN (airplane-direction a)) 180]
                                   [(string=? LEFT (airplane-direction a)) 90])
                                   AIRPLANE)
                     (* GRID-SIZE (posn-x (airplane-position a)))
                     (* GRID-SIZE (posn-y (airplane-position a)))
                     "left"
                     "top"
                     bg))

(check-expect (render-airplane airplane11 SCENE) (place-image/align
                                            AIRPLANE
                                            (* GRID-SIZE 10)
                                            (* GRID-SIZE 10)
                                            "left"
                                            "top"
                                            SCENE))
(check-expect (render-airplane airplane12 SCENE) (place-image/align
                                            (rotate 270 AIRPLANE)
                                            (* GRID-SIZE 10)
                                            (* GRID-SIZE 10)
                                            "left"
                                            "top"
                                            SCENE))
(check-expect (render-airplane airplane13 SCENE) (place-image/align
                                            (rotate 180 AIRPLANE)
                                            (* GRID-SIZE 10)
                                            (* GRID-SIZE 10)
                                            "left"
                                            "top"
                                            SCENE))
(check-expect (render-airplane airplane14 SCENE) (place-image/align
                                            (rotate 90 AIRPLANE)
                                            (* GRID-SIZE 10)
                                            (* GRID-SIZE 10)
                                            "left"
                                            "top"
                                            SCENE))

; List-of-fires Image -> Image
; put fires onto the background
(define (render-fires alof bg)
  (cond [(empty? alof) bg]
        [else (place-image/align FIRE
                                 (* GRID-SIZE (posn-x (first alof)))
                                 (* GRID-SIZE (posn-y (first alof)))
                                 "left"
                                 "top"
                                 (render-fires (rest alof) bg))]))

(check-expect (render-fires lof0 SCENE) SCENE)
(check-expect (render-fires lof1 SCENE) (place-image/align FIRE
                                                     (* 5 GRID-SIZE)
                                                     (* 10 GRID-SIZE)
                                                     "left"
                                                     "top"
                                                     SCENE))
(check-expect (render-fires lof2 SCENE) (place-image/align FIRE
                                                     (* 5 GRID-SIZE)
                                                     (* 10 GRID-SIZE)
                                                     "left"
                                                     "top"
                                                     (place-image/align FIRE
                                                                        (* 10 GRID-SIZE)
                                                                        (* 15 GRID-SIZE)
                                                                        "left"
                                                                        "top"
                                                                        SCENE)))
; Game -> Image
; render the current pic of the game
(define (render g)
  (render-airplane (game-airplane g)
                   (render-fires (game-fires g) SCENE)))

(check-expect (render game1) (render-airplane airplane11 (render-fires lof0 SCENE)))
(check-expect (render game2) (render-airplane airplane11 (render-fires lof1 SCENE)))
(check-expect (render game3) (render-airplane airplane11 (render-fires lof2 SCENE)))
                                                     
                                                     

; Airplane -> Airplane
; airplane a flys upwards 1 grid
; every tick towards it's current direction
(define (tock-airplane a)
  (make-airplane
   (make-posn (cond [(string=? LEFT (airplane-direction a)) (max (sub1 (posn-x (airplane-position a))) 0)]
                    [(string=? RIGHT (airplane-direction a)) (min (add1 (posn-x (airplane-position a))) MAX-X)]
                    [else (posn-x (airplane-position a))])
              (cond [(string=? UP (airplane-direction a)) (max (sub1 (posn-y (airplane-position a))) 0)]
                    [(string=? DOWN (airplane-direction a)) (min (add1 (posn-y (airplane-position a))) MAX-X)]
                    [else (posn-y (airplane-position a))]))
   (airplane-direction a)))


(check-expect (tock-airplane airplane11) (make-airplane (make-posn 10 9) UP))
(check-expect (tock-airplane airplane12) (make-airplane (make-posn 11 10) RIGHT))
(check-expect (tock-airplane airplane13) (make-airplane (make-posn 10 11) DOWN))
(check-expect (tock-airplane airplane14) (make-airplane (make-posn 9 10) LEFT))
(check-expect (tock-airplane airplane21) airplane21)
(check-expect (tock-airplane airplane22) airplane22)
(check-expect (tock-airplane airplane23) airplane23)
(check-expect (tock-airplane airplane24) airplane24)

; Game -> Game
; generate next tick's game state
(define (tock g)
  (make-game (tock-airplane (game-airplane g))
                 (game-fires g)))

(check-expect (tock game1) (make-game (tock-airplane airplane11) lof0))
(check-expect (tock game2) (make-game (tock-airplane airplane11) lof1))
(check-expect (tock game3) (make-game (tock-airplane airplane11) lof2))


; Airplane KeyEvent -> Airplane
; use arrow keys to change airplane's flying direction
(define (control-airplane a ke)
  (make-airplane
   (airplane-position a)
   (cond [(string=? ke "up") UP]
         [(string=? ke "right") RIGHT]
         [(string=? ke "down") DOWN]
         [(string=? ke "left") LEFT]
         [else (airplane-direction a)])))

(check-expect (control-airplane airplane11 UP) (make-airplane (make-posn 10 10) UP))
(check-expect (control-airplane airplane11 RIGHT) (make-airplane (make-posn 10 10) RIGHT))
(check-expect (control-airplane airplane11 DOWN) (make-airplane (make-posn 10 10) DOWN))
(check-expect (control-airplane airplane11 LEFT) (make-airplane (make-posn 10 10) LEFT))
(check-expect (control-airplane airplane11 "k") airplane11)

; Airplane List-of-fires -> List-of-fires
; try to put out fire if the current place of airplane is on fire
(define (try-put-out a lof)
  (cond [(empty? lof) '()]
        [else (if (equal? (airplane-position a) (first lof))
                  (try-put-out a (rest lof))
                  (cons (first lof) (try-put-out a (rest lof))))]))

(check-expect (try-put-out airplane11 lof0) lof0)
(check-expect (try-put-out (make-airplane (make-posn 10 15) UP) lof2) lof1)

; Game KeyEvent -> KeyEvent
; - use arrow keys to control airplane's flying direction
; - use space key to pour water to put out a fire
(define (control-game g ke)
  (make-game (control-airplane (game-airplane g) ke)
             (if (string=? ke " ")
                 (try-put-out (game-airplane g) (game-fires g))
                 (game-fires g)) ))

(check-expect (control-game game1 "right") (make-game (make-airplane (make-posn 10 10) RIGHT) lof0))
(check-expect (control-game game4 " ") (make-game (make-airplane (make-posn 10 15) UP) lof1))

; Number -> List-of-fires
; generate c fires randomly and add them to lof
(define (set-fire c lof)
  (cond [(= c 0) lof]
        [else (set-fire (sub1 c)
                        (cons (make-posn (random MAX-X) (random MAX-Y)) lof))]))

(check-expect (length (set-fire 5 '())) 5)
(check-expect (length (set-fire 3 '())) 3)

(define (game-main count)
  (big-bang (make-game (make-airplane (make-posn 0 0) RIGHT) (set-fire count '()))
    [on-draw render]
    [on-tick tock 0.5]
    [on-key control-game]))

(game-main 20)


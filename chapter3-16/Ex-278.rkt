#lang htdp/isl

(require 2htdp/image)
(require 2htdp/universe)

(define WORM_SIZE 5)
(define UNIT_SIZE (* WORM_SIZE 2))
(define WIDTH_COUNT 50)
(define HEIGHT_COUNT 50)

(define WORM (circle WORM_SIZE "solid" "red"))
(define FOOD (circle WORM_SIZE "solid" "green"))
(define BACKGROUND (empty-scene (* WIDTH_COUNT WORM_SIZE 2)
                                (* HEIGHT_COUNT WORM_SIZE 2)))

; A Direction is one of:
; - "up"
; - "right"
; - "down"
; - "left"

; A List-of-posns is one of:
; - (cons Posn '())
; - (cons Posn List-of-posns)
; interpretation the body of the worm,
; the first item in the list is worm's head.

(define posns1 (list (make-posn 3 4)))
(define posns2 (list (make-posn 3 4) (make-posn 3 5) (make-posn 3 6)))

(define-struct worm [direction positions])
; A Worm is a structure:
; (make-worm Direction Posn)
; interpretaion (make-worm d p) represents
; a worm at p and moving towards direction p

(define worm1 (make-worm "up" posns1))
(define worm2 (make-worm "up" posns2))


(define-struct game [worm food])
; A Game is a structure:
; (make-game Worm Posn)
; interpretation (make-game w p) represents
; a worm and a food at position p

(define game1 (make-game worm1 (make-posn 10 20)))
(define game2 (make-game worm1 (make-posn 3 3)))
(define game3 (make-game worm2 (make-posn 3 3)))

; Game -> Image
; put the image of worm and food in game onto the background
(define (render g)
  (render-food (game-food g)
               (render-worm (worm-positions (game-worm g)))))


; Posn Image -> Image
; put f onto the image s
(define (render-food f s)
  (place-image/align FOOD
                     (* UNIT_SIZE (posn-x f))
                     (* UNIT_SIZE (posn-y f))
                     "left"
                     "top"
                     s))

(check-expect (render-food (make-posn 3 3) BACKGROUND)
              (place-image/align FOOD
                                 (* UNIT_SIZE 3)
                                 (* UNIT_SIZE 3)
                                 "left"
                                 "top"
                                 BACKGROUND))
                                                                    


; [List-of Posn] -> Image
; put the image of worm onto the background
(define (render-worm alop)
  (local (; Posn Image -> Image
          ; render every posn in alop as WORM onto s
          (define (fn cur prev)
            (place-image/align WORM
                               (* UNIT_SIZE (posn-x cur))
                               (* UNIT_SIZE (posn-y cur))
                               "left"
                               "top"
                               prev))
          )
    (foldr fn BACKGROUND alop)))

(check-expect (render-worm posns1)
              (place-image/align WORM
                           (* UNIT_SIZE 3)
                           (* UNIT_SIZE 4)
                           "left"
                           "top"
                           BACKGROUND))

(check-expect (render-worm posns2)
              (place-image/align WORM
                           (* UNIT_SIZE 3)
                           (* UNIT_SIZE 6)
                           "left"
                           "top"
                           (place-image/align WORM
                                        (* UNIT_SIZE 3)
                                        (* UNIT_SIZE 5)
                                        "left"
                                        "top"
                                        (place-image/align WORM
                                                     (* WORM_SIZE 2 3)
                                                     (* WORM_SIZE 2 4)
                                                     "left"
                                                     "top"
                                                     BACKGROUND))))

; Posn -> Posn 
; generate a posn not equal to the origin p
; which x coordinate is less than WIDTH_COUNT
; y coordinate is less than HEIGHT_COUNT
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
     p (make-posn (random WIDTH_COUNT) (random HEIGHT_COUNT))))
 
; Posn Posn -> Posn 
; generative recursion 
; check if the candidate is equal to p,
; if equal, generate another until not equal
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))


; Game -> Game
; For each clock tick, 
; - if food is at the next position the head is, worm eat
;   the food and longer than past by one diameter, and another
;   food generate randomly on the scene
; - otherwise, the worm should just move a diameter.
(define (tock g)
  (cond [(equal? (game-food g) (next-p (first (worm-positions (game-worm g))) (worm-direction (game-worm g))))
         (make-game (eat-food (game-worm g) (game-food g)) (food-create (game-food g)))]
        [else (make-game (tock-worm (game-worm g)) (game-food g) )]))

(check-expect (tock game1) (make-game (tock-worm (game-worm game1)) (game-food game1)))
(check-expect (game-worm (tock game2)) (game-worm (make-game (make-worm "up" (list (make-posn 3 3) (make-posn 3 4))) (make-posn 5 9))))

; Worm Food -> Worm
; worm eat food and become one more diameter logner
(define (eat-food w f)
  (make-worm (worm-direction w)
             (cons f (worm-positions w))))

(check-expect (eat-food worm1 (make-posn 8 7)) (make-worm "up" (cons (make-posn 8 7) posns1)))
(check-expect (eat-food worm2 (make-posn 9 9)) (make-worm "up" (cons (make-posn 9 9) posns2)))


; Worm -> Worm
; For each clock tick, the worm should move a diameter.
(define (tock-worm w)
  (make-worm (worm-direction w)
             (cons (next-p (first (worm-positions w)) (worm-direction w))
                   (remove-last (worm-positions w)))))

(check-expect (tock-worm worm1) (make-worm "up" (list (make-posn 3 3))))
(check-expect (tock-worm worm2) (make-worm "up" (list (make-posn 3 3) (make-posn 3 4) (make-posn 3 5))))

; Posn Direction -> Posn
; get next position according to current position and direction
(define (next-p p d)
  (cond [(string=? "up" d) (make-posn (posn-x p) (sub1 (posn-y p)))]
        [(string=? "down" d) (make-posn (posn-x p) (add1 (posn-y p)))]
        [(string=? "left" d) (make-posn (sub1 (posn-x p)) (posn-y p))]
        [(string=? "right" d) (make-posn (add1 (posn-x p)) (posn-y p))]))

(check-expect (next-p (make-posn 3 5) "up") (make-posn 3 4))
(check-expect (next-p (make-posn 3 5) "down") (make-posn 3 6))
(check-expect (next-p (make-posn 3 5) "left") (make-posn 2 5))
(check-expect (next-p (make-posn 3 5) "right") (make-posn 4 5))


; List-of-posns -> List-of-posns
; remove last item in alop
(define (remove-last alop) (reverse (rest (reverse alop))))

(check-expect (remove-last posns1) '())
(check-expect (remove-last posns2) (list (make-posn 3 4) (make-posn 3 5)))

; Game String -> Worm
; change the moving direction of the worm according to which key is stroke
(define (control g key)
  (cond [(or (string=? "up" key)
             (string=? "right" key)
             (string=? "down" key)
             (string=? "left" key))
         (make-game (make-worm key (worm-positions (game-worm g))) (game-food g))]
         [else g]))

(check-expect (control game1 "a") game1)
(check-expect (control game1 "down") (make-game (make-worm "down" posns1) (make-posn 10 20)))
(check-expect (control game3 "up") (make-game (make-worm "up" posns2) (make-posn 3 3)))

; Number Nubmer Number -> Boolean;
; determine if x is smaller than (not equal) min
; or larger than (not equal) max
(define (out-range x min max)
  (or (< x min) (> x max)))

(check-expect (out-range 10 0 100) #false)
(check-expect (out-range 0 0 100) #false)
(check-expect (out-range 100 0 100) #false)
(check-expect (out-range -1 0 100) #true)
(check-expect (out-range 101 0 100) #true)


; Game -> Boolean
; determine if game is end
(define (fail-game g)
  (fail (game-worm g)))

; Worm -> Boolean
; determine if the game is end
; - crash on the wall
; - crash on the worm itself
(define (fail w)
  (or (fail-self w) (fail-wall w)))

(check-expect (fail (make-worm "up" (list (make-posn -1 0)))) #true)
(check-expect (fail (make-worm "right" (list (make-posn 0 -1)))) #true)
(check-expect (fail (make-worm "up" (list (make-posn 101 100)))) #true)
(check-expect (fail (make-worm "left" (list (make-posn 100 101)))) #true)
(check-expect (fail (make-worm "left" (list (make-posn 20 30)))) #false)

(check-expect (fail (make-worm "up" (list (make-posn 2 1) (make-posn 3 1) (make-posn 3 0) (make-posn 2 0) (make-posn 1 0)))) #false)

; Worm -> Boolean
; - crash on the wall
(define (fail-wall w)
  (or (out-range (posn-x (first (worm-positions w))) 0 WIDTH_COUNT)
      (out-range (posn-y (first (worm-positions w))) 0 HEIGHT_COUNT)))


(check-expect (fail-wall (make-worm "down" (list (make-posn -1 0)))) #true)
(check-expect (fail-wall (make-worm "left" (list (make-posn 0 -1)))) #true)
(check-expect (fail-wall (make-worm "up" (list (make-posn (+ WIDTH_COUNT 1) HEIGHT_COUNT)))) #true)
(check-expect (fail-wall (make-worm "left" (list (make-posn WIDTH_COUNT (+ HEIGHT_COUNT 1))))) #true)


; Worm -> Boolean
; - crash on the worm itself
(define (fail-self w)
  (member? (first (worm-positions w)) (rest (worm-positions w))))

(check-expect (fail-self (make-worm "up" (list (make-posn 2 1) (make-posn 3 1) (make-posn 3 0) (make-posn 2 0) (make-posn 1 0)))) #false)

; Game -> Image
; when the game end, render text to prompt
(define (render-fail g) (place-image/align
                           (text (if (fail-wall (game-worm g)) "worm hit border" "worm hit itself") 16 "red")
                           0
                           (image-height BACKGROUND)
                           "left"
                           "bottom"
                           (render g)))


(define (worm-main rate seperate)
  (length (worm-positions (game-worm
                           (big-bang (make-game (make-worm "right" (list (make-posn 2 5))) (make-posn 10 10))
    [on-draw render]
    [on-tick tock rate]
    [on-key control]
    [stop-when fail-game render-fail])))))

; (worm-main 0.15 #false)
#lang htdp/bsl

; distances in terms of pixels:
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH  400)
(define CENTER (quotient WIDTH 2))
 
(define-struct game [left-player right-player ball])
 
(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))

(game-ball game0)
#|
(game-ball (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))
(make-posn CENTER CENTER)
|#

(posn? (game-ball game0))
#|
(posn? (game-ball (make-game MIDDLE MIDDLE (make-posn CENTER CENTER))))
(posn? (make-posn CENTER CENTER))
#true
|#

(game-left-player game0)
#|
(game-left-player (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))
MIDDLE
(quotient HEIGHT 2)
100
|#
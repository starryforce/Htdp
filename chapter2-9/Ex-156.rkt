#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot 


; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
 
; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

(check-expect (tock '()) '())
(check-expect (tock (cons 2 '())) (cons 1 '()))
(check-expect (tock (cons 3 (cons 4 '()))) (cons 2 (cons 3 '())))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh (cons 4 '()) " ") (cons HEIGHT (cons 4 '())))
(check-expect (keyh (cons 4 (cons 50 '())) " ") (cons HEIGHT (cons 4 (cons 50 '()))))
 
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

(check-expect (to-image '()) BACKGROUND)
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 9 (cons 50 '())))
              (place-image SHOT XSHOTS 9 (place-image SHOT XSHOTS 50 BACKGROUND)))

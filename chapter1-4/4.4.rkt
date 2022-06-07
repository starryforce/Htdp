#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A WorldState is a Number.
; interpretation number of pixels between the top and the UFO
 
(define WIDTH 300) ; distances in terms of pixels 
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay (circle 10 "solid" "green") (ellipse 60 30 "solid" "grey")))
 
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
     [on-tick nxt]
     [to-draw render/status]))
 
; WorldState -> WorldState
; computes next location of UFO 
(check-expect (nxt 11) 14)
(define (nxt y)
  (+ y 3))
 
; WorldState -> Image
; places UFO at given height into the center of MTSCN
(check-expect (render 11) (place-image UFO 150 11 MTSCN))
(define (render y)
  (place-image UFO (/ WIDTH 2) y MTSCN))

; WorldState -> Image
; adds a status line to the scene created by render  
 
(check-expect (render/status 10)
              (place-image (text "descending" 11 "green")
                           10 10
                           (render 10)))
 
(define (render/status y)
  (cond
    [(<= 0 y CLOSE)
     (place-image (text "descending" 11 "green")
                  10 10
                  (render y))]
    [(and (< CLOSE y) (<= y HEIGHT))
     (place-image (text "closing in" 11 "orange")
                  10 10
                  (render y))]
    [(> y HEIGHT)
     (place-image (text "landed" 11 "red")
                  10 10
                  (render y))]))
#lang htdp/bsl

(require 2htdp/image)


(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))



; An LR (short for launching rocket) is one of:
; – "resting"
; – NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight

; given: 150
; expect: (place-image ROCKET 25 150 BACKGROUND)
; given: "resting"
; expect: (place-image ROCKET 25 200 BACKGROUND)
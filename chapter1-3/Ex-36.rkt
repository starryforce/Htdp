#lang htdp/bsl
 (require 2htdp/image)

; we use numbers to represent number of pixels
; Image -> Number
; count the number of pixels of image

;(define (image-area img) 100)

; given: (rectangle 100 50 "solid" "black")
; expect: 5000

; given: (circle 5 "solid" "red")
; expect: 100


;(define (image-area img)
;  (... img ...))

(define (image-area img)
  (* (image-width img) (image-height img)))

(image-area (rectangle 100 50 "solid" "black"))
(image-area (circle 5 "solid" "red"))
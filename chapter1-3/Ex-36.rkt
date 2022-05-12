;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-36) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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
#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define VALUE-MAX 100)
(define VALUE-MIN 0)
(define DESC-SPEED 0.1)
(define ADD-SPEED 1/3)
(define MINUS-SPEED 1/5)

(define SCENE-HEIGHT 200)
(define SCENE-WIDTH 50)

(define BACKGROUND
  (empty-scene (+ SCENE-WIDTH 2) (+ SCENE-HEIGHT 2)))

; A Happiness is a number in [0,100]
; interpretation: the percentage of the
; height of the scene

; Happiness -> Image
; render a image inside the BACKGROUND with
; same width and Happiness% of SCENE-WIDTH
(define (render hp)
  (place-image/align (rectangle SCENE-WIDTH (* SCENE-HEIGHT (/ hp VALUE-MAX)) "solid" "red")
                   (+ (/ SCENE-WIDTH 2) 1) (+ SCENE-HEIGHT 1) "center" "bottom"
                   BACKGROUND))

; Happiness -> Happiness
; add DESC-SPEED to hp to change the happiness
(define (tock hp)
  (normalize (- hp DESC-SPEED)))

(check-expect (tock 100) 99.9)
(check-expect (tock 10) 9.9)
(check-expect (tock 0) 0)
(check-expect (tock 0.05)  0)
(check-expect (tock 0.1) 0)


; Happiness key -> Happiness
; figure width down arrow and up arrow
; Every time the down arrow key is pressed,
; Happiness decreases by 1/5;
; every time the up arrow is pressed,
; happiness jumps by 1/3.
; else keep hp

(define (on-key-press hp key)
  (cond [(key=? key "up") (normalize (* hp (+ 1 ADD-SPEED)))]
        [(key=? key "down") (normalize (* hp (- 1 MINUS-SPEED)))]
        [else hp]))

(check-expect (on-key-press 50 "down") 40)
(check-expect (on-key-press 3 "up") 4)
(check-expect (on-key-press 30 "up")  40)
(check-expect (on-key-press 90 "up") 100)
(check-expect (on-key-press 99 "up") 100)

; Happiness -> Happiness
; when hp > 100, return 100
; when hp < 0, return 0
; else return hp
(define (normalize hp)
  (cond [(> hp VALUE-MAX) VALUE-MAX]
        [(< hp VALUE-MIN) VALUE-MIN]
        [else hp]))

(check-expect (normalize -0.5) 0)
(check-expect (normalize 55) 55)
(check-expect (normalize 105) 100)


; Happiness -> Happiness
; launches the programe from some initial state
(define (main hp)
  (big-bang hp
    [to-draw render]
    [on-tick tock]
    [on-key on-key-press]))


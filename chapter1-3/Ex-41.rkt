;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-41) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

; car roof
(define ROOF (rectangle (* 6 WHEEL-RADIUS) WHEEL-RADIUS "solid" "red"))
; car body
(define BODY (rectangle (+ WHEEL-DISTANCE (* 2 WHEEL-RADIUS)) (* 2 WHEEL-RADIUS) "solid" "red"))
; car main
(define MAIN (above ROOF BODY))

; car wheels
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle (* 1.5 WHEEL-RADIUS) WHEEL-RADIUS (* 2 WHEEL-RADIUS) "transparent"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR (underlay/align/offset "middle" "bottom" MAIN  0 WHEEL-RADIUS BOTH-WHEELS))


; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
; examples: 
;   given: 20, expect 23
;   given: 78, expect 81
(define (tock cw)
  (+ cw 3))

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

;(check-expect (tock 78) 80)

(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define HEIGHT-CAR (image-height CAR))
(define WIDTH-CAR (image-width CAR))
(define BACKGROUND (place-image tree 200 (- 50 (/ (image-width tree) 2)) (empty-scene 500 50)))
(define Y-CAR (- 50 (/ HEIGHT-CAR 2)))



HEIGHT-CAR
WIDTH-CAR

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 
(define (render cw)
  (place-image CAR (+ cw (/ WIDTH-CAR 2)) Y-CAR BACKGROUND))

(define (end cw)
  (> cw (image-width BACKGROUND)))


; WorldState -> WorldState
; launches the program from some initial state 
; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]
     [stop-when end]))

(main 13)
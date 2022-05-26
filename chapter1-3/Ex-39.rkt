;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ex-39) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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

#lang htdp/bsl

(require 2htdp/image)

; N Image -> Image
; produces a column—a vertical arrangement—of n copies of img.
(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (above img (col (sub1 n) img))]))

; N Image -> Image
; produces a row—a horizontal arrangement—of n copies of img
(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (beside img (row (sub1 n) img))]))

(define SIZE 10)
(define COLUMNS 10)
(define ROWS 10)

(define UNIT (square SIZE "outline" "black"))
(define SCENE (empty-scene (* COLUMNS SIZE) (* ROWS SIZE)))
(define BACK (place-image/align (col 10 (row 10 UNIT)) 0 0 "left" "top" SCENE))

(define BALLOON (circle 3 "solid" "red"))

; A NPosn is (make-posn N N)
; A List-of-NPosn is one of:
; - '()
; - (cons NPosn List-of-NPosn

(define ex10 '())
(define ex11 (cons (make-posn 10 20) '()))
(define ex12 (cons (make-posn 20 40) ex11))
(define ex13 (cons (make-posn 10 20) ex11))

; List-of-NPosn -> Image
; produces an image of the lecture hall with red dots added as specified by the Posns.
(define (add-balloons alop)
  (cond [(empty? alop) BACK]
        [else (place-image
               BALLOON
               (posn-x (first alop))
               (posn-y (first alop))
               (add-balloons (rest alop)))]))

(check-expect (add-balloons ex10) BACK)
(check-expect (add-balloons ex11) (place-image BALLOON 10 20 BACK))
(check-expect (add-balloons ex12) (place-image BALLOON 20 40 (place-image BALLOON 10 20 BACK)))
(check-expect (add-balloons ex13) (place-image BALLOON 10 20 (place-image BALLOON 10 20 BACK)))




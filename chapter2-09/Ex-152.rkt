#lang htdp/bsl

(require 2htdp/image)

(define ex1 (rectangle 50 30 "solid" "red"))
(define ex2 (circle 10 "solid" "green"))

; N Image -> Image
; produces a column—a vertical arrangement—of n copies of img.
(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (above img (col (sub1 n) img))]))

(check-expect (col 0 ex1) empty-image)
(check-expect (col 3 ex1) (above ex1 ex1 ex1))
(check-expect (col 2 ex2) (above ex2 ex2))

; N Image -> Image
; produces a row—a horizontal arrangement—of n copies of img
(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (beside img (row (sub1 n) img))]))

(check-expect (row 0 ex1) empty-image)
(check-expect (row 3 ex1) (beside ex1 ex1 ex1))
(check-expect (row 2 ex2) (beside ex2 ex2))
#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

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
(define SCENE-WIDTH (* COLUMNS SIZE))
(define SCENE-HEIGHT (* ROWS SIZE))

(define BALLOON (circle 3 "solid" "red"))
(define UNIT (square SIZE "outline" "black"))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define BACK (place-image/align (col ROWS (row COLUMNS UNIT)) 0 0 "left" "top" SCENE))

; A NPosn is (make-posn N N)
; A List-of-posns is one of:
; - '()
; - (cons NPosn List-of-posns

; List-of-posns -> Image
; produces an image of the lecture hall with red dots added as specified by the Posns.
(define (add-balloons alop)
  (cond [(empty? alop) BACK]
        [else (place-image
               BALLOON
               (posn-x (first alop))
               (posn-y (first alop))
               (add-balloons (rest alop)))]))


(define-struct pair [balloon# lob])

; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; Pair -> Image
(define (render p) (add-balloons (pair-lob p)))

; Pair -> Pair
; add a new posn to (pair-lob p)
; the new posn is random in the scene
(define (tock p) (make-pair
                  (pair-balloon# p)
                  (cons
                   (create-balloon SCENE-WIDTH SCENE-HEIGHT)
                   (pair-lob p))))

(define (create-balloon max-x max-y)
  (make-posn (random max-x) (random max-y)))

; Number -> List-of-posns
(define (riot n)
  (pair-lob
   (big-bang (make-pair n '())
     (on-draw render)
     (on-tick tock 1 n))))


#lang htdp/isl+

(require 2htdp/image)

#| Exercise 480.
Design render-queens.
The function consumes a natural number n, a list of QPs, and an Image.
It produces an image of an n by n chess board with the given image placed according to the given QPs.

You may wish to look for an image for a chess queen on-line or
create a simplistic one with the available image functions.
|#

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

(define SIZE 20)
(define QUEEN-IMAGE (star (/ SIZE 2) 'solid "orange"))


; N [List-of QP] Image -> Image
; produces an image of an n by n chess board
; with the given image placed according to the given QPs aloqp.
(define (render-queens n aloqp img)
  (foldr (lambda (cur prev) (above cur prev))
         empty-image
         (build-list
          n
          (lambda (y)
            (local (
                    ; Boolean -> Image
                    ; render an outline square,
                    ; a chess in it if isFill is #t
                    (define (render-square isFill)
                      (overlay (if isFill img empty-image)
                               (square SIZE 'outline "black")))
                    ; N -> Image
                    ; generate a row contains n squares side by side
                    (define row
                      (foldr (lambda (cur prev) (beside cur prev))
                             empty-image
                             (build-list n (lambda (x) (render-square (member? (make-posn x y) aloqp)))))))
              row)))))

(render-queens QUEENS (list (make-posn 1 2) (make-posn 3 6)) QUEEN-IMAGE)
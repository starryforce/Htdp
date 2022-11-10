#lang htdp/bsl+

(require 2htdp/image)

; All Polygons belongs to NELop,which is what the function accepts
; connect-dots share the same input data defination with last

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
  (render-line (connect-dots img p) (first p) (last p)))
 
; Image NELoP -> Image
; connects the Posns in p in an image
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
 
; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))


(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p) (make-posn 10 20))
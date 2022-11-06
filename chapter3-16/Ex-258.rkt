#lang htdp/isl

(require 2htdp/image)

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
  (local (; Image NELoP -> Image
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
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red")))
    ; - IN -
    (render-line (connect-dots img p) (first p) (last p))))

 
; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))
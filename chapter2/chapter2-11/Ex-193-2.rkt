#lang htdp/bsl+

(require 2htdp/image)

; a plain background image 
(define MT (empty-scene 50 50))

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

; All Polygons belongs to NELop,which is what the function accepts
; connect-dots share the same input data defination with last

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
  (connect-dots img (add-at-end (first p) p)))

(check-expect
  (render-poly MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))
 
; Image NELoP -> Image
; connects the Posns in p in an image
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))

(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 10 10 20 10 "red")
                20 10 20 20 "red")
               20 20 10 20 "red"))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

; Posn Polygon -> Polygon
; add the first item of p to the end of p
(define (add-at-end pos p)
  (cond [(empty? p) (list pos)]
        [else (cons (first p)
                    (add-at-end pos (rest p)))]))

(check-expect (add-at-end (make-posn 1 2) triangle-p)
              (list (make-posn 20 10)
                    (make-posn 20 20)
                    (make-posn 30 20)
                    (make-posn 1 2) ))
#lang htdp/bsl+

(require 2htdp/image)

; a plain background image 
(define MT (empty-scene 50 50))

; All Polygons belongs to NELop,which is what the function accepts
; connect-dots share the same input data defination with last

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
  (connect-dots img p (first p)))

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
(define (connect-dots img p pos)
  (cond
    [(empty? (rest p)) (render-line img (first p) pos)]
    [else (render-line (connect-dots img (rest p) pos)
                       (first p)
                       (second p))]))

(check-expect (connect-dots MT triangle-p (first triangle-p))
              (scene+line
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red")
              30 20 20 10 "red"))

(check-expect (connect-dots MT square-p (first square-p))
              (scene+line
              (scene+line
               (scene+line
                (scene+line MT 10 10 20 10 "red")
                20 10 20 20 "red")
               20 20 10 20 "red")
              10 20 10 10 "red"))
 
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
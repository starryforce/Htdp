#lang htdp/isl

(require 2htdp/image)

; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))


; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

(define (fold2 l initial g)
  (cond
    [(empty? l) initial]
    [else
     (g
      (first l)
      (fold2 (rest l) initial g))]))

(define (product-from-abstract l)
  (fold2 l 1 *))

(define ex10 '())
(define ex11 (list 1))
(define ex12 (list 1 2 3))

(check-expect (product ex10) (product-from-abstract ex10))
(check-expect (product ex11) (product-from-abstract ex11))
(check-expect (product ex12) (product-from-abstract ex12))

(define (image*-from-abstract l)
  (fold2 l emt place-dot))

(define ex20 '())
(define ex21 (list (make-posn 1 2)))
(define ex22 (list (make-posn 1 2) (make-posn 10 20) (make-posn 20 40)))

(check-expect (image* ex20) (image*-from-abstract ex20))
(check-expect (image* ex21) (image*-from-abstract ex21))
(check-expect (image* ex22) (image*-from-abstract ex22))

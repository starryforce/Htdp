#lang htdp/isl+

(require 2htdp/image)

; Exercise 525. 
; Tackle the wish list that the skeleton implies: Design the three functions.

(define SCENE-SIZE 200)
(define BACK (empty-scene SCENE-SIZE SCENE-SIZE))
(define PA (make-posn 50 150))
(define PB (make-posn 150 150))
(define PC (make-posn 100 63.4))

; Image Posn Posn Posn -> Image 
; adds the black triangle a, b, c to scene
(check-expect (add-triangle BACK PA PB PC) (scene+line
                               (scene+line
                                (scene+line
                                 BACK
                                 50 150 150 150 "black")
                                100 63.4 50 150 "black")
                               100 63.4 150 150 "black"))
(define (add-triangle scene a b c) (scene+line
                                    (scene+line
                                     (scene+line
                                      scene
                                      (posn-x a) (posn-y a) (posn-x b) (posn-y b) "black")
                                     (posn-x b) (posn-y b) (posn-x c) (posn-y c) "black")
                                    (posn-x c) (posn-y c) (posn-x a) (posn-y a) "black"))
(define threshold 10)
; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided
(check-expect (too-small? PA PB PC) #f)
(define (too-small? a b c)
  (local ((define distance
            (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
                     (sqr (- (posn-y a) (posn-y b)))))))
    (< distance threshold)))
 
; Posn Posn -> Posn 
; determines the midpoint between a and b
(check-expect (mid-point (make-posn 0 0) (make-posn 100 100)) (make-posn 50 50))
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))


; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to scene0, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles of scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))

(add-sierpinski MT A B C)
(add-sierpinski BACK PA PB PC)
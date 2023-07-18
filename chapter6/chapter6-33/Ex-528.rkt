#lang htdp/isl+

(require 2htdp/image)

; Posn Posn -> Posn
; determine the mid point of a & b
(check-expect (mid-point (make-posn 0 0) (make-posn 100 100)) (make-posn 50 50))
(check-expect (mid-point (make-posn 100 0) (make-posn 200 0)) (make-posn 150 0))
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))

; Posn Posn Posn -> Boolean
; determine if the traingle a b c is small enough
(define (too-small? a b)
  (and (<= (abs (- (posn-x a) (posn-x b))) 2)
       (<= (abs (- (posn-y a) (posn-y b))) 2)))

(define background (empty-scene 200 200))
(define start0 (make-posn 20 10))
(define end0 (make-posn 180 100))
(define observe0 (make-posn 80 180))

; Image Posn Posn Posn
; Bézier curves 
(define (smooth-curve scene start end observe)
  (cond [(too-small? start end) (scene+line scene
                                            (posn-x start)
                                            (posn-y start)
                                            (posn-x end)
                                            (posn-y end) "red")]
        [else (local ((define A-B (mid-point start observe))
                      (define B-C (mid-point end observe))
                      (define A-B-C (mid-point A-B B-C))
                      (define scene1 (smooth-curve scene start A-B-C A-B))
                      (define scene2 (smooth-curve scene1 A-B-C end B-C)))
                scene2)]))


(smooth-curve background start0 end0 observe0)
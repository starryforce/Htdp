#lang htdp/isl+
(require 2htdp/image)

; Exercise 527
; a fractal Savannah tree

(define background (empty-scene 200 200))
(define LINE-COLOR "dark blue")

; Image Number Number Number Number -> Image
; generative adds the line to scene0. the line's start point
; is (make-posn base-x base-y), and length is len with rotate a
; uses the two intermediate points as the new starting points for two lines.
; The lengths and the angles of the two branches change in a fixed manner,
; but independently of each other
; stop if the line is too short
(define (add-savannah scene0 base-x base-y len a)
  (cond [(too-short? len) scene0]
        [else
         (local ((define scene1 (add-rotate-line scene0 base-x base-y len a))
                 (define start-left (end-point base-x base-y (* len (/ 1 3)) a))
                 (define scene2 (add-savannah scene1
                                                 (posn-x start-left)
                                                 (posn-y start-left)
                                                 (* len (/ 3 5))
                                                 (- a 15)))
                 (define start-right (end-point base-x base-y (* len (/ 2 3)) a))
                 (define scene3 (add-savannah scene2
                                                 (posn-x start-right)
                                                 (posn-y start-right)
                                                 (* len (/ 4 5))
                                                 (+ a 20)))
                 )
           scene3)]))

; Number -> Boolean
; determine if the length of line is shorter than the threshold
(define (too-short? len) (< len 8))

; Image Number Number Number Number -> Image
; add a line to scene0,
; the line's start point is (make-posn base-x base-y),
; and length is len with rotate a
(check-expect (add-rotate-line background 100 100 100 0) (scene+line background 100 100 200 100 LINE-COLOR))
(check-expect (add-rotate-line background 100 100 100 90) (scene+line background 100 100 100 200 LINE-COLOR))
(check-expect (add-rotate-line background 100 100 100 180) (scene+line background 100 100 0 100 LINE-COLOR))
(check-expect (add-rotate-line background 100 100 100 270) (scene+line background 100 100 100 0 LINE-COLOR))
(define (add-rotate-line scene0 base-x base-y len a)
  (local ((define end (end-point base-x base-y len a)))
    (scene+line scene0 base-x base-y (posn-x end) (posn-y end) LINE-COLOR)))

; N N N N -> Posn
; calc the end point
(define (end-point start-x start-y r a)
  (local ((define (normalize x) (inexact->exact (round x))))
    (make-posn (normalize (+ start-x (* r (cos (* pi (/ a 180))))))
               (normalize (+ start-y (* r (sin (* pi (/ a 180)))))))))

(add-savannah background 100 200 100 270)

#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define WORM_SIZE 5)
(define SPEED (* WORM_SIZE 2))
(define WIDTH_COUNT 100)
(define HEIGHT_COUNT 100)

(define WORM (circle WORM_SIZE "solid" "red"))
(define BACKGROUND (empty-scene (* WIDTH_COUNT WORM_SIZE) (* HEIGHT_COUNT WORM_SIZE)))

; A Direction is one of:
; - "up"
; - "right"
; - "down"
; - "left"

(define-struct worm [direction position])
; A Worm is a structure:
; (make-worm Direction Posn)
; interpretaion (make-worm d p) represents
; a worm at p and moving towards direction p

(define worm1 (make-worm "up" (make-posn 3 4)))
(define worm2 (make-worm "right" (make-posn 5 8)))

; Worm -> Image
; place the image of w on the background
(define (render w)
  (place-image WORM
               (+ (* WORM_SIZE 2 (posn-x (worm-position w))) WORM_SIZE)
               (+ (* WORM_SIZE 2 (posn-y (worm-position w))) WORM_SIZE)
               BACKGROUND))

(check-expect (render worm1)
              (place-image WORM
                           (+ (* WORM_SIZE 2 3) WORM_SIZE)
                           (+ (* WORM_SIZE 2 4) WORM_SIZE)
                           BACKGROUND))
(check-expect (render worm2)
              (place-image WORM
                           (+ (* WORM_SIZE 2 5) WORM_SIZE)
                           (+ (* WORM_SIZE 2 8) WORM_SIZE)
                           BACKGROUND))


; Worm -> Worm
; For each clock tick, the worm should move a diameter.
(define (tock w)
  (make-worm (worm-direction w)
             (make-posn (cond [(string=? (worm-direction w) "left") (sub1 (posn-x (worm-position w)))]
                              [(string=? (worm-direction w) "right") (add1 (posn-x (worm-position w)))]
                              [else (posn-x (worm-position w))])
                        (cond [(string=? (worm-direction w) "up") (sub1 (posn-y (worm-position w)))]
                              [(string=? (worm-direction w) "down") (add1 (posn-y (worm-position w)))]
                              [else (posn-y (worm-position w))]))))

(check-expect (tock worm1) (make-worm "up" (make-posn 3 3)))
(check-expect (tock worm2) (make-worm "right" (make-posn 6 8)))

; Worm String -> Worm
; change the moving direction of the worm according to which key is stroke
(define (control w key)
  (cond [(or (string=? "up" key)
             (string=? "right" key)
             (string=? "down" key)
             (string=? "left" key))
         (make-worm key (worm-position w))]
         [else w]))

(check-expect (control worm1 "a") worm1)
(check-expect (control worm1 "down") (make-worm "down" (make-posn 3 4)))
(check-expect (control worm2 "up") (make-worm "up" (make-posn 5 8)))

; Worm -> Boolean
; end the game if the worm out of the bounder
(define (fail w)
  (or (< (posn-x (worm-position)) 0 ...
       (posn-y (worm-position))))

(check-expect (fail (make-worm "up" (make-posn 3 4))) #false)
(check-expect (fail (make-worm "up" (make-posn 100 100))) #false)
(check-expect (fail (make-worm "up" (make-posn 0 0))) #false)
(check-expect (fail (make-worm "up" (make-posn -1 4))) #true)
(check-expect (fail (make-worm "up" (make-posn -1 101))) #true)

(define (worm-main rate)
  (big-bang (make-worm "right" (make-posn 5 5))
    [on-draw render]
    [on-tick tock rate]
    [on-key control]))

;(worm-main 0.1)

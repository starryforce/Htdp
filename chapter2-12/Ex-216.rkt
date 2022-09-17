#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define WORM_SIZE 5)
(define WIDTH_COUNT 50)
(define HEIGHT_COUNT 40)

(define WORM (circle WORM_SIZE "solid" "red"))
(define BACKGROUND (empty-scene (* WIDTH_COUNT WORM_SIZE 2) (* HEIGHT_COUNT WORM_SIZE 2)))

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
(define (fail w) (or (out-range (posn-x (worm-position w)) 0 WIDTH_COUNT)
                     (out-range (posn-y (worm-position w)) 0 HEIGHT_COUNT)))

(check-expect (fail (make-worm "up" (make-posn 3 4))) #false)
(check-expect (fail (make-worm "up" (make-posn 50 40))) #false)
(check-expect (fail (make-worm "up" (make-posn 0 0))) #false)
(check-expect (fail (make-worm "up" (make-posn -1 4))) #true)
(check-expect (fail (make-worm "up" (make-posn -1 101))) #true)

; Number Nubmer Number -> Boolean;
; determine if x is smaller than (not equal) min
; or larger than (not equal) max
(define (out-range x min max)
  (or (< x min) (> x max)))

(check-expect (out-range 10 0 100) #false)
(check-expect (out-range 0 0 100) #false)
(check-expect (out-range 100 0 100) #false)
(check-expect (out-range -1 0 100) #true)
(check-expect (out-range 101 0 100) #true)

; Worm -> Image
; when the game end, render text to prompt
(define (render-fail w) (place-image/align
                           (text "worm hit border" 16 "red")
                           0
                           (image-height (render w))
                           "left"
                           "bottom"
                           (render w)))
  

(define (worm-main rate)
  (big-bang (make-worm "right" (make-posn 5 5))
    [on-draw render]
    [on-tick tock rate]
    [on-key control]
    [stop-when fail render-fail]))

;`(worm-main 0.1)

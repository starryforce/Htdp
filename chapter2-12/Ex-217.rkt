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

; A List-of-posns is one of:
; - (cons Posn '())
; - (cons Posn List-of-posns)
; interpretation the body of the worm,
; the first item in the list is worm's head.

(define posns1 (list (make-posn 3 4)))
(define posns2 (list (make-posn 3 4) (make-posn 3 5) (make-posn 3 6)))

(define-struct worm [direction positions])
; A Worm is a structure:
; (make-worm Direction Posn)
; interpretaion (make-worm d p) represents
; a worm at p and moving towards direction p

(define worm1 (make-worm "up" posns1))
(define worm2 (make-worm "up" posns2))

; Worm -> Image
; put the image of worm onto the background
(define (render w) (render-worm(worm-positions w)))


; List-of-posns -> Image
; put the image of worm onto the background
(define (render-worm alop)
  (cond [(empty? alop) BACKGROUND]
        [else (place-image WORM
                           (+ (* WORM_SIZE 2 (posn-x (first alop))) WORM_SIZE)
                           (+ (* WORM_SIZE 2 (posn-y (first alop))) WORM_SIZE)
                           (render-worm (rest alop)))]))


(check-expect (render worm1)
              (place-image WORM
                           (+ (* WORM_SIZE 2 3) WORM_SIZE)
                           (+ (* WORM_SIZE 2 4) WORM_SIZE)
                           BACKGROUND))

(check-expect (render worm2)
              (place-image WORM
                           (+ (* WORM_SIZE 2 3) WORM_SIZE)
                           (+ (* WORM_SIZE 2 6) WORM_SIZE)
                           (place-image WORM
                                        (+ (* WORM_SIZE 2 3) WORM_SIZE)
                                        (+ (* WORM_SIZE 2 5) WORM_SIZE)
                                        (place-image WORM
                                                     (+ (* WORM_SIZE 2 3) WORM_SIZE)
                                                     (+ (* WORM_SIZE 2 4) WORM_SIZE)
                                                     BACKGROUND))))
; Worm -> Worm
; For each clock tick, the worm should move a diameter.
(define (tock w)
  (make-worm (worm-direction w)
             (cons (next-p (first (worm-positions w)) (worm-direction w))
                   (remove-last (worm-positions w)))))

(check-expect (tock worm1) (make-worm "up" (list (make-posn 3 3))))
(check-expect (tock worm2) (make-worm "up" (list (make-posn 3 3) (make-posn 3 4) (make-posn 3 5))))

; Posn Direction -> Posn
; get next position according to current position and direction
(define (next-p p d)
  (cond [(string=? "up" d) (make-posn (posn-x p) (sub1 (posn-y p)))]
        [(string=? "down" d) (make-posn (posn-x p) (add1 (posn-y p)))]
        [(string=? "left" d) (make-posn (sub1 (posn-x p)) (posn-y p))]
        [(string=? "right" d) (make-posn (add1 (posn-x p)) (posn-y p))]))

(check-expect (next-p (make-posn 3 5) "up") (make-posn 3 4))
(check-expect (next-p (make-posn 3 5) "down") (make-posn 3 6))
(check-expect (next-p (make-posn 3 5) "left") (make-posn 2 5))
(check-expect (next-p (make-posn 3 5) "right") (make-posn 4 5))


; List-of-posns -> List-of-posns
; remove last item in alop
(define (remove-last alop) (reverse (rest (reverse alop))))

(check-expect (remove-last posns1) '())
(check-expect (remove-last posns2) (list (make-posn 3 4) (make-posn 3 5)))

; Worm String -> Worm
; change the moving direction of the worm according to which key is stroke
(define (control w key)
  (cond [(or (string=? "up" key)
             (string=? "right" key)
             (string=? "down" key)
             (string=? "left" key))
         (make-worm key (worm-positions w))]
         [else w]))

(check-expect (control worm1 "a") worm1)
(check-expect (control worm1 "down") (make-worm "down" posns1))
(check-expect (control worm2 "up") (make-worm "up" posns2))

(define (worm-main rate)
  (big-bang (make-worm "right" (list (make-posn 9 5) (make-posn 8 5) (make-posn 7 5) (make-posn 6 5) (make-posn 5 5) (make-posn 4 5) (make-posn 3 5) (make-posn 2 5)))
    [on-draw render]
    [on-tick tock rate]
    [on-key control]))


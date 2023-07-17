#lang htdp/isl+

(require 2htdp/image)

(define L "left")
(define R "right")

; A BoatPosition is one of:
; - L
; - R

(define-struct puzzle [leftm leftc boat rightm rightc])
; A PuzzleState is a structure:
; (make-puzzle Number Number BoatPosition Number Number)
; representation (make-puzzle lm lc bp rm rc)
; lm & lc represent the number of missionaries and cannibals on left bank,
; rm & rc represent the number of missionaries and cannibals on right bank,
; bp is the boat position, either on the left bank or the right bank.

(define initial-puzzle (make-puzzle 3 3 L 0 0))
(define final-puzzle (make-puzzle 0 0 R 3 3))
(define intermediate-puzzle (make-puzzle  1 1 L 2 2))

; PuzzleState -> Boolean
; detects whether in a given state all people are on the right river bank
(check-expect (final? final-puzzle) #t)
(define (final? p) (equal? p final-puzzle))

(define PersonSize 20)
(define M (circle PersonSize "solid" "black")) ; missionaries
(define C (circle PersonSize "outline" "black")) ; cannibals
(define B (rectangle (* 4 PersonSize) (* 2 PersonSize) "solid" "brown")) ; boat
; PuzzleState -> Image
; maps a state of the missionary-and-cannibal puzzle to an image
(define (render-mc p)
  (beside (render-bank (puzzle-leftm p) (puzzle-leftc p))
          (render-boat (puzzle-boat p))
          (render-bank (puzzle-rightm p) (puzzle-rightc p))))

; Image Number -> Image
; put count image, up and down
(define (render-p count image)
  (cond [(zero? count) empty-image]
        [else (above image
                     (render-p (sub1 count) image))]))

; Number Number -> Image
; render one side of bank, m & c represent the number of missionaries and cannibals
(define (render-bank m c)
  (overlay  (beside/align "top" (render-p m M)
                          (render-p c C))
            (rectangle (* 5 PersonSize) (* 7 PersonSize) "outline" "black")))

; BoatPosition -> Image
(define (render-boat p)
  (overlay/align p "middle"
                 B
                 (rectangle (* 9 PersonSize) (* 7 PersonSize) "outline" "black")))


(render-mc initial-puzzle)
(render-mc final-puzzle)
(render-mc intermediate-puzzle)
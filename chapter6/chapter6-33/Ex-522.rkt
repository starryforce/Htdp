#lang htdp/isl+

#| Exercise 522.
Modify the representation from exercise 521 so that
a state records the sequence of states traversed to get there.
Use a list of states.

Articulate and write down an accumulator statement with
the data definition that explains the additional field.

Modify final? or render-mc for this representation as needed.
|#

(require 2htdp/image)

(define L "left")
(define R "right")

; A BoatPosition is one of:
; - L
; - R

(define-struct puzzle [leftm leftc boat rightm rightc])
; A Puzzle is a structure:
; (make-puzzle Number Number BoatPosition Number Number)
; representation (make-puzzle lm lc bp rm rc)
; lm & lc represent the number of missionaries and cannibals on left bank,
; rm & rc represent the number of missionaries and cannibals on right bank,
; bp is the boat position, either on the left bank or the right bank.

; A PuzzleState is [List-of Puzzle]:
; the first item is it's current status.
; the rest are the history from initial state to current.
; from latest to farest.

(define initial-puzzle (list (make-puzzle 3 3 L 0 0)))
(define final-puzzle (list (make-puzzle 0 0 R 3 3) (make-puzzle  1 1 L 2 2) (make-puzzle 3 3 L 0 0)))
(define intermediate-puzzle (list (make-puzzle  1 1 L 2 2) (make-puzzle 3 3 L 0 0)) )

; PuzzleState -> Boolean
; detects whether in a given state all people are on the right river bank
(check-expect (final? final-puzzle) #t)
(define (final? p) (equal? (first p) (make-puzzle 0 0 R 3 3)))

(define PersonSize 20)
; missionaries image
(define M (circle PersonSize "solid" "black")) 
; cannibals image
(define C (circle PersonSize "outline" "black")) 
; boat image
(define B (rectangle (* 4 PersonSize) (* 2 PersonSize) "solid" "brown")) 

; PuzzleState -> Image
; maps a state of the missionary-and-cannibal puzzle to an image
(define (render-mc ps)
  (local ((define p (first ps)))
    (beside (render-bank (puzzle-leftm p) (puzzle-leftc p))
          (render-boat (puzzle-boat p))
          (render-bank (puzzle-rightm p) (puzzle-rightc p)))))

; Number Image -> Image
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
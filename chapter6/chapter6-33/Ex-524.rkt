#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

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

; query the current status of p
(define (current-puzzle p) (first p))
; query the history of p
(define (history-puzzle p) (rest p))

; PuzzleState -> Boolean
; detects whether in a given state all people are on the right river bank
(check-expect (final? final-puzzle) #t)
(define (final? p) (equal? (current-puzzle p) (make-puzzle 0 0 R 3 3)))

(define TR '((0 1 0 -1)
             (0 2 0 -2)
             (1 0 -1 0)
             (1 1 -1 -1)
             (2 0 -2 0)))

(check-expect (create-next-states (list (list (make-puzzle 3 3 L 0 0))))
              (list (list (make-puzzle 3 2 R 0 1) (make-puzzle 3 3 L 0 0))
                    (list (make-puzzle 3 1 R 0 2) (make-puzzle 3 3 L 0 0))
                    (list (make-puzzle 2 2 R 1 1) (make-puzzle 3 3 L 0 0))))
; [List-of PuzzleState] -> [List-of PuzzleState]
; generates the list of all those states that a boat ride can reach.
(define (create-next-states alop)
  (local ((define (generate pl)
            (local ((define p (current-puzzle pl))
                    (define boat (puzzle-boat p))
                    (define matrix (map (lambda (x) (map (lambda (y) (* (if (equal? boat L) -1 1) y)) x)) TR)))
              (map (lambda (r) (cons (make-puzzle (+ (puzzle-leftm p) (first r))
                                                  (+ (puzzle-leftc p) (second r))
                                                  (if (equal? boat L) R L)
                                                  (+ (puzzle-rightm p) (third r))
                                                  (+ (puzzle-rightc p) (fourth r))) pl))
                   matrix)))
          (define (valid? pf)
            (local ((define p (first pf)))
              (and (>= (puzzle-leftm p) 0)
                   (>= (puzzle-leftc p) 0)
                   (>= (puzzle-rightm p) 0)
                   (>= (puzzle-rightc p) 0)
                   (or (>= (puzzle-leftm p) (puzzle-leftc p)) (zero? (puzzle-leftm p)))
                   (or (>= (puzzle-rightm p) (puzzle-rightc p)) (zero? (puzzle-rightm p)))
                   )))
          (define (new? p)
            (not (member? (current-puzzle p) (history-puzzle p)))))
    (filter new? (filter valid? (foldr append '() (map generate alop))))))


; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination ???
 
(check-expect (first (solve initial-puzzle)) (first final-puzzle))
 
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (first (filter final? los))]
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))

(define PersonSize 20)
; missionaries image
(define M (circle PersonSize "solid" "black")) 
; cannibals image
(define C (circle PersonSize "outline" "black")) 
; boat image
(define B (rectangle (* 4 PersonSize) (* 2 PersonSize) "solid" "brown")) 

; PuzzleState -> Image
; maps a state of the missionary-and-cannibal puzzle to an image
(define (render-mc p)
  (beside (render-bank (puzzle-leftm p) (puzzle-leftc p))
          (render-boat (puzzle-boat p))
          (render-bank (puzzle-rightm p) (puzzle-rightc p))))

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

(define (main x)
  (local ((define result (solve initial-puzzle))
          (define movie (map render-mc result)))
    (run-movie 5 (reverse movie))))
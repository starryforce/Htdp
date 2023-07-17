#lang htdp/isl+

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
            (local ((define p (first pl))
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
            (not (member? (first p) (rest p)))))
    (filter new? (filter valid? (foldr append '() (map generate alop))))))

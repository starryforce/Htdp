#lang htdp/isl+

(require 2htdp/abstraction)

; Exercise 483
; a Board contains the list of positions where a queen has been placed;

(define (n-queens n)
  (local (; Board -> [List-of QP]
          ; finds spots where it is still safe to place a queen
          (define (find-open-spots a-board)
            (local ((define all (for*/list ((x n) (y n))
                                  (make-posn x y)))
                    (define (fn p)
                      (andmap (lambda (x) (not (threatening? x p))) a-board)))
              (filter fn all)))
          ; Board N -> [Maybe [List-of QP]]
          ; places n queens on board; otherwise, returns #false
          (define (place-queens a-board n)
            (cond [(= n 0) '()]
                  [else (local (; still safe spots
                                (define candidates (find-open-spots a-board)))
                          (foldl (lambda (cur prev)
                                   (if (boolean? prev)
                                       (local ((define result (place-queens (add-queen a-board cur) (sub1 n))))
                                         (if (boolean? result) result (cons cur result)))
                                       prev))
                                 #false
                                 candidates)
                          )])))
    (place-queens (board0 n) n)))

; N -> Board 
; creates the initial n by n board
(define (board0 n) '())
 
; Board QP -> Board 
; places a queen at qp on a-board
(define (add-queen a-board qp)
  (cons qp a-board))

; QP QP -> Boolean
; determine if q1 & q2 threatening each other.
(define (threatening? q1 q2)
  (cond [(= (posn-y q1) (posn-y q2)) #t]
        [(= (posn-x q1) (posn-x q2)) #t]
        [(= (+ (posn-x q1) (posn-y q1)) (+ (posn-x q2) (posn-y q2))) #t]
        [(= (- (posn-x q1) (posn-x q2)) (- (posn-y q1) (posn-y q2))) #t]
        [else #f]))
 


; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))

; N -> [List-of QP] -> Boolean
; produces a predicate on queen placements that
; determines whether a given placement is a solution to an n queens puzzle
(define (n-queens-solution? n)
  (lambda (aloqp)
    (and (= (length aloqp) n)
         (andmap (lambda (qp)
                   (local ((define others (remove qp aloqp)))
                     (andmap (lambda (o) (not (threatening? qp o)))
                             others)))
                 aloqp))))

(check-satisfied 4QUEEN-SOLUTION-2 (n-queens-solution? 4))
(check-satisfied (n-queens 4) (n-queens-solution? 4))

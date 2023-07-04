#lang htdp/isl+

(require 2htdp/abstraction)

; Exercise 483
; a Board collects those positions where a queen can still be placed;

(define (n-queens n)
  (place-queens (board0 n) n))

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
                )]))



(define board2 (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 1 0)
                     (make-posn 1 1)))
(define board3 (list (make-posn 0 0)
                     (make-posn 0 1)
                     (make-posn 0 2)
                     (make-posn 1 0)
                     (make-posn 1 1)
                     (make-posn 1 2)
                     (make-posn 2 0)
                     (make-posn 2 1)
                     (make-posn 2 2)))
; N -> Board 
; creates the initial n by n board
(define (board0 n)
  (for*/list ((x n) (y n))
    (make-posn x y)))

(check-expect (board0 1) (list (make-posn 0 0)))
(check-expect (board0 2) board2)
 
; Board QP -> Board 
; places a queen at qp on a-board
(define (add-queen a-board qp)
  (local ((define (t? p) (not (threatening? qp p))))
    (filter t? a-board)))
(check-expect (add-queen board2 (make-posn 0 1)) '())
(check-expect (add-queen board3 (make-posn 0 0)) (list (make-posn 1 2) (make-posn 2 1))) 
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board) a-board)
(check-expect (find-open-spots board2) board2)


; QP QP -> Boolean
; determine if q1 & q2 threatening each other.
(define (threatening? q1 q2)
  (cond [(= (posn-y q1) (posn-y q2)) #t]
        [(= (posn-x q1) (posn-x q2)) #t]
        [(= (+ (posn-x q1) (posn-y q1)) (+ (posn-x q2) (posn-y q2))) #t]
        [(= (- (posn-x q1) (posn-x q2)) (- (posn-y q1) (posn-y q2))) #t]
        [else #f]))

(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
(define 4QUEEN-SOLUTION-WRONG
  (list  (make-posn 0 2) (make-posn 0 1)
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


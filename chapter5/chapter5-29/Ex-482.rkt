#lang htdp/isl+

; Exercise 482

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

; N -> Board 
; creates the initial n by n board
(define (board0 n) ...)
 
; Board QP -> Board 
; places a queen at qp on a-board
(define (add-queen a-board qp)
  a-board)
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  '())
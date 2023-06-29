#lang htdp/isl+

#| Exercise 479.
Design the threatening? function.
It consumes two QPs and determines whether
queens placed on the two respective squares would threaten each other.
|#

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

; QP QP -> Boolean
; determine if q1 & q2 threatening each other.
(define (threatening? q1 q2)
  (cond [(= (posn-y q1) (posn-y q2)) #t]
        [(= (posn-x q1) (posn-x q2)) #t]
        [(= (+ (posn-x q1) (posn-y q1)) (+ (posn-x q2) (posn-y q2))) #t]
        [(= (- (posn-x q1) (posn-x q2)) (- (posn-y q1) (posn-y q2))) #t]
        [else #f]))

; horizontals : have the same y-coordinate
(check-expect (threatening? (make-posn 1 2) (make-posn 3 2)) #t)
; verticals : have the same x-coordinate
(check-expect (threatening? (make-posn 1 2) (make-posn 1 3)) #t)
; diagonals left-top -> right-bottom : the differences between the two coordinates remain the same
(check-expect (threatening? (make-posn 1 2) (make-posn 2 3)) #t)
; diagonals left-bottom -> right-top : have coordinates whose sums are the same
(check-expect (threatening? (make-posn 1 4) (make-posn 4 1)) #t)
(check-expect (threatening? (make-posn 1 0) (make-posn 0 3)) #f)
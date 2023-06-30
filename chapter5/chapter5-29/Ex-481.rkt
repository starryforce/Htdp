#lang htdp/isl+

; data example: [List-of QP]
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
(check-satisfied 4QUEEN-SOLUTION-WRONG (n-queens-solution? 4))

; QP QP -> Boolean
; determine if q1 & q2 threatening each other.
(define (threatening? q1 q2)
  (cond [(= (posn-y q1) (posn-y q2)) #t]
        [(= (posn-x q1) (posn-x q2)) #t]
        [(= (+ (posn-x q1) (posn-y q1)) (+ (posn-x q2) (posn-y q2))) #t]
        [(= (- (posn-x q1) (posn-x q2)) (- (posn-y q1) (posn-y q2))) #t]
        [else #f]))
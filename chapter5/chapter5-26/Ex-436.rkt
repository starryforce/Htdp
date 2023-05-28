#lang htdp/isl+

; Exercise 436. Formulate a termination argument for food-create from exercise 432.

; Posn -> Posn 
; generate a posn not equal to the origin p
; which x coordinate is less than WIDTH_COUNT
; y coordinate is less than HEIGHT_COUNT
; termination due to the MAX, posn is limit, every time evaluate food-create.
; the alternative posn reduce 1
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (local ((define MAX 50)
          (define alter (make-posn (random MAX) (random MAX)))
          )
    (if (equal? p alter) (food-create p) alter)))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
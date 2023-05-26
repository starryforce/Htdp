#lang htdp/isl+

#| Exercise 432.
Exercise 219 introduces the function food-create,
which consumes a Posn and produces a randomly chosen Posn
that is guaranteed to be distinct from the given one.
First reformulate the two functions as a single definition,using local;
then justify the design of food-create.
|#



; Posn -> Posn 
; generate a posn not equal to the origin p
; which x coordinate is less than WIDTH_COUNT
; y coordinate is less than HEIGHT_COUNT
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
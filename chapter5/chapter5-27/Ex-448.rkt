#lang htdp/isl+

#| Exercise 448.
The find-root algorithm terminates for all (continuous) f, left, and right for which the assumption holds.
Why? Formulate a termination argument.

Hint Suppose the arguments of find-root describe an interval of size S1.
How large is the distance between left and right for the first and second recursive call to find-root?
After how many steps is (- right left) smaller than or equal to ε?
|#

; termination Suppose the arguments of find-root describe an interval of size S1
; every recursion shrinks the range to half of the S1
; 1st recursion: (/ 2 S1) 
; 2st recursion: (/ 4 S1)
; (expt S1 (- (/ 1 (expt 2 t)))) < ε
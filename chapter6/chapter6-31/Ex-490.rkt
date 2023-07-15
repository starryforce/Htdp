#lang htdp/isl+

#| Exercise 490.
Develop a formula that describes the abstract running time of relative->absolute.
Hint Evaluate the expression (relative->absolute (build-list size add1)) by hand.
Start by replacing size with 1, 2, and 3. How many recursions of relative->absolute and add-to-each are required each time? 
|#

; '(1)
; (relative->absolute '()) 
; (add-to-each 1 rest-of-l) 1(call) times


; '(1 2)
; (relative->absolute '(2))
; (add-to-each 1 rest-of-l) 1(call) + 1(recursion) times
; --
; (relative->absolute '())
; (add-to-each 2 rest-of-l) 1(call) times


; '(1 2 3)
; (relative->absolute '(2 3))
; (add-to-each 1 rest-of-l) 1(call) + 2(recursion) times
; --
; (relative->absolute '(3))
; (add-to-each 2 rest-of-l) 1(call) + 1(recursion) times
; --
; (relative->absolute '())
; (add-to-each 3 rest-of-l) 1(call) times

; (+ n  (/ (* n (n + 1)) 2))
; (/ (+ (* n n) (* 3 n)) 2)
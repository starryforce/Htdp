#lang htdp/isl+

#| Exercise 520.
The solve* function generates all states reachable with n boat trips
before it looks at states that require n + 1 boat trips,
even if some of those boat trips return to previously encountered states.
Because of this systematic way of traversing the tree,
solve* cannot go into an infinite loop. Why?
Terminology This way of searching a tree or a graph is dubbed breadth-first search.
|#

; if the puzzle have an answer, it will always finally go into trivally case.
; but if the puzzle doesn't have an answer, it will got into infinite loop.


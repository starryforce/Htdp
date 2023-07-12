#lang htdp/isl+

#| Exercise 498.
Complete height.v3. Hint The bottom-most tree of figure 184
contains no subtree to the left of the subtree marked with 1.
It contains one complete path from root to tree in the part of
the tree that is to the left of the subtree marked with 2;
this path consists of two steps.
|#
(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))


; Tree -> N
; measures the height of abt0
(check-expect (height.v2 example) 3)
(define (height.v2 abt0)
  (local (; Tree N N -> N
          ; measures the height of abt
          ; accumulator s is the number of steps 
          ; it takes to reach abt from abt0
          ; accumulator m is the maximal height of
          ; the part of abt0 that is to the left of abt
          (define (h/a abt s m)
            (cond
              [(empty? abt) ...]
              [else
               (... (h/a (node-left abt) ... s ... ... m ...)
                    (h/a (node-right abt) ... s ... ... m ...) ...)])))
    (h/a abt0 ... ... )))
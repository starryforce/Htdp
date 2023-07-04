#lang htdp/isl+

; Exercise 485

(define-struct tree [left right])
; A Tree is a structure
; (make-tree NT NT)
; representation (make-tree l r) is a number tree with l & r

; A NT is one of:
; - Number
; - Tree

(define nt0 10)
(define nt1 (make-tree 1 4))
(define nt2 (make-tree (make-tree 3 8) 10))

; NT -> Number
; determines the sum of the numbers in t
(define (sum-tree t)
  (cond [(number? t) t]
        [else (+ (sum-tree (tree-left t))
                 (sum-tree (tree-right t)))]))

(check-expect (sum-tree nt0) 10)
(check-expect (sum-tree nt1) 5)
(check-expect (sum-tree nt2) 21)

; O(n) n is the number of nodes.

; the number of nodes

; except the lowest level, all other level are pairs of number tree.

; a number
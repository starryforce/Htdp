#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define node10 (make-node 10 'a NONE NONE))
(define node24 (make-node 24 'b NONE NONE))
(define node77 (make-node 77 'c NONE NONE))
(define node99 (make-node 99 'd NONE NONE))
(define node15 (make-node 15 'e node10 node24))
(define node95 (make-node 95 'f NONE node99))
(define node29 (make-node 29 'g node15 NONE))
(define node89 (make-node 89 'h node77 node95))
(define node63 (make-node 63 'i node29 node89))


; Number BST -> [Symbol | NONE]
; If the tree contains a node whose ssn field is n,
; the function produces the value of the name field in that node.
; Otherwise, the function produces NONE.
(define (search-bst n bst)
  (cond [(no-info? bst) NONE]
        [else (cond [(= n (node-ssn bst)) (node-name bst)]
                    [(> n (node-ssn bst)) (search-bst n (node-right bst))]
                    [else (search-bst n (node-left bst))])]))

(check-expect (search-bst 15 node63) 'e)
(check-expect (search-bst 63 node63) 'i)
(check-expect (search-bst 39 node63) NONE)
(check-expect (search-bst 39 NONE) NONE)
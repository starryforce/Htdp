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

(define node16-r (make-node 16 'j NONE NONE))
(define node24-r (make-node 24 'b node16-r NONE))
(define node15-r (make-node 15 'e node10 node24-r))
(define node29-r (make-node 29 'g node15-r NONE))
(define node63-r (make-node 63 'i node29-r node89))

; BST N S -> BST
; produces a BST that is just like B
; and that in place of one NONE subtree contains the node structure
(define (create-bst B N S)
  (cond [(no-info? B) (make-node N S NONE NONE)]
        [else (cond [(> N (node-ssn B))
                     (make-node (node-ssn B)
                                (node-name B)
                                (node-left B)
                                (create-bst (node-right B) N S))]
                    [else
                     (make-node (node-ssn B)
                                (node-name B)
                                (create-bst (node-left B) N S)
                                (node-right B))])]))


(check-expect (create-bst node63 16 'j) node63-r)
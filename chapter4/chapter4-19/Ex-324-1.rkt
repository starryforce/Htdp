#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define node10 (make-node 10 'a NONE NONE))
(define node24 (make-node 24 'a NONE NONE))
(define node77 (make-node 77 'a NONE NONE))
(define node99 (make-node 99 'a NONE NONE))
(define node15 (make-node 15 'a node10 node24))
(define node95 (make-node 95 'a NONE node99))
(define node29 (make-node 29 'a node15 NONE))
(define node89 (make-node 89 'a node77 node95))
(define node63 (make-node 63 'a node29 node89))


; BT -> [List-of Number]
; produces the sequence of all the ssn numbers in bt
; as they show up from left to right when looking at a tree drawing.
(define (inorder bt)
  (cond [(no-info? bt) '()]
        [else (append (inorder (node-left bt))
                      (list (node-ssn bt))
                      (inorder (node-right bt)))]))

(check-expect (inorder node63) '(10 15 24 29 63 77 89 95 99))

; it produces a asceding list contains all ssn in tree.
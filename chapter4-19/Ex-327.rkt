#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define node10 (make-node 10 'h NONE NONE))
(define node24 (make-node 24 'i NONE NONE))
(define node77 (make-node 77 'l NONE NONE))
(define node99 (make-node 99 'o NONE NONE))
(define node15 (make-node 15 'd node10 node24))
(define node95 (make-node 95 'g NONE node99))
(define node29 (make-node 29 'b node15 NONE))
(define node89 (make-node 89 'c node77 node95))
(define node63 (make-node 63 'a node29 node89))

(define input '((99 o)
                (77 l)
                (24 i)
                (10 h)
                (95 g)
                (15 d)
                (89 c)
                (29 b)
                (63 a)))

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

; [List-of [List Number Symbol]] -> BST
; It consumes a list of numbers and names and produces a BST by repeatedly applying create-bst
(define (create-bst-from-list l)
  (cond [(empty? l) NONE]
        [else (create-bst (create-bst-from-list (rest l))
                          (first (first l))
                          (second (first l)))]))


(check-expect (create-bst-from-list input) node63)


(define (create-bst-from-list-from-foldr l)
  (local (; [List Number Symbol] BST -> BST
          ; insert cur into prev
          (define (fn cur prev)
            (create-bst prev
                        (first cur)
                        (second cur)))
          )
    (foldr fn NONE l)))

(check-expect (create-bst-from-list-from-foldr input) node63)


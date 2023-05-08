#lang htdp/isl+

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define node24 (make-node 24 'i NONE NONE))
(define node15 (make-node 15 'd NONE node24))
(define node87 (make-node 87 'h NONE NONE))
(define node25 (make-node 25 'd node87 NONE))


; Number BT -> Boolean
; determines whether n occurs in BT. 
(define (contains-bt? n BT)
  (cond [(no-info? BT) #false]
        [else (or (= n (node-ssn BT))
                  (contains-bt? n (node-left BT))
                  (contains-bt? n (node-right BT)))]))


; Number BT -> [Maybe Symbol]
; If the tree contains a node structure whose ssn field is n,
; the function produces the value of the name field in that node.
; Otherwise, the function produces #false.
(define (search-bt n BT)
  (if (contains-bt? n BT)
      (cond [(no-info? BT) #false]
            [else (if (= n (node-ssn BT))
                      (node-name BT)
                      (if (false? (search-bt n (node-left BT)))
                          (search-bt n (node-right BT))
                          (search-bt n (node-left BT)))
                      )])
      #false))

(check-expect (search-bt 24 node15) 'i)
(check-expect (search-bt 24 node24) 'i)
(check-expect (search-bt 30 node24) #false)
(check-expect (search-bt 87 node87) 'h)
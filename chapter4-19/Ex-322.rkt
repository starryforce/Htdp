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

(check-expect (contains-bt? 24 node15) #true)
(check-expect (contains-bt? 16 node15) #false)
(check-expect (contains-bt? 87 node87) #true)
(check-expect (contains-bt? 19 node87) #false)
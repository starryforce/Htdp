#lang htdp/isl+

#| Exercise 390.
Design the function tree-pick.
The function consumes a tree of symbols and a list of directions:

Clearly a Direction tells the function whether to choose the left or the right branch in a nonsymbolic tree.
What is the result of the tree-pick function?
Don’t forget to formulate a full signature.
The function signals an error when given a symbol and a non-empty path.
|#

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
(define ex1 'a)
(define ex2 (make-branch 'b 'c))
(define ex3 (make-branch ex1 ex2))
 
; A Direction is one of:
; – 'left
; – 'right
(define ex-p1 '())
(define ex-p2 '(right))
(define ex-p3 '(right left))
 
; A list of Directions is also called a path.

(define ERROR "no such path")

; TOS [List-of Direction] -> TOS
; follow the path of alod to find the related symbol in atos.
; if not a valid path ,signals an error
(define (tree-pick atos alod)
  (cond [(and (symbol? atos) (empty? alod)) atos]
        [(and (symbol? atos) (cons? alod)) (error ERROR)]
        [(and (branch? atos) (empty? alod)) atos]
        [(and (branch? atos) (cons? alod))
         (cond [(symbol=? 'left (first alod)) (tree-pick (branch-left atos) (rest alod))]
               [(symbol=? 'right (first alod)) (tree-pick (branch-right atos) (rest alod))])]))

; (tree-pick atos (rest alod)) ; (make-branch 'b 'c)
; (tree-pick (branch-left atos) alod) ; error
; (tree-pick (branch-left atos) (rest alod)) 'b
; (tree-pick (branch-right atos) alod) ; error
; (tree-pick (branch-right atos) (rest alod)) 'c

(check-expect (tree-pick (make-branch 'b 'c) '(right)) 'c)


(check-expect (tree-pick ex1 ex-p1) ex1)
(check-error (tree-pick ex1 ex-p2) ERROR)
(check-expect (tree-pick ex2 ex-p1) ex2)
(check-expect (tree-pick ex2 ex-p2) 'c)
(check-expect (tree-pick ex3 ex-p3) 'b)
(check-expect (tree-pick ex3 ex-p2) ex2)
(check-error (tree-pick ex2 ex-p3) ERROR)
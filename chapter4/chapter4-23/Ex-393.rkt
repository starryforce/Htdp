#lang htdp/isl+

#| Exercise 393.
Figure 62 presents two data definitions for finite sets.
Design the union function for the representation of finite sets of your choice.
It consumes two sets and produces one that contains the elements of both.

Design intersect for the same set representation.
It consumes two sets and produces the set of exactly those elements that occur in both.
|#

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

; [List-of Son.R] [List-of Son.R] -> [List-of Son.R]
; produces one that contains the elements of both alos1 & alos2
(define (union alos1 alos2)
  (cond [(empty? alos1) alos2]
        [else (cond [(member? (first alos1) alos2) (union (rest alos1) alos2)]
                    [else (cons (first alos1) (union (rest alos1) alos2))])]))

(check-expect (union '() '()) '())
(check-expect (union '(1) '()) '(1))
(check-expect (union '() '(2)) '(2))
(check-expect (union '(1) '(2)) '(1 2))

; [List-of Son.R] [List-of Son.R] -> [List-of Son.R]
; produces the set of exactly those elements that occur in both alos1 & alos2
(define (intersect alos1 alos2)
  (cond [(empty? alos1) '()]
        [else (cond [(member? (first alos1) alos2)
                     (cons (first alos1) (intersect alos1 (rest alos2)))]
                    [else (intersect (rest alos1) alos2)])]))

(check-expect (intersect '() '()) '())
(check-expect (intersect '() '(2)) '())
(check-expect (intersect '(1) '()) '())
(check-expect (intersect '(1) '(2)) '())
(check-expect (intersect '(1 2) '(2 3)) '(2))
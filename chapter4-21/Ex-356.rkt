#lang htdp/isl+

#| Exercise 356.
Extend the data representation of Interpreting Variables to include the application of a programmer-defined function.
Recall that a function application consists of two pieces: a name and an expression.
The former is the name of the function that is applied; the latter is the argument.

Represent these expressions: (k (+ 1 1)), (* 5 (k (+ 1 1))), (* (i 5) (k (+ 1 1))).
We refer to this newly defined class of data as BSL-fun-expr. 
|#

(define-struct add [left right])
; An Add is a structure:
; (make-add BSL-fun-expr BSL-fun-expr)
; interpreation (make-add a b) represent sum of a & b

(define-struct mul [left right])
; A Mul is a structure:
; (make-mul BSL-fun-expr BSL-fun-expr)
; interpretation (make-mul a b) represent muliply of a & b

(define-struct fn [name arg])
; An Fn is a structure:
; (make-fn Symbol BSL-fun-expr)
; interpretation (make-fn n a) represent the application of function n to a

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fn Symbol BSL-fun-expr)

; (k (+ 1 1))
(make-fn 'k (make-add 1 1))

; (* 5 (k (+ 1 1)))
(make-mul 5 (make-fn 'k (make-add 1 1)))

; (* (i 5) (k (+ 1 1)))

(make-mul (make-fn 'i 5) (make-fn 'k (make-add 1 1 )))

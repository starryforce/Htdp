#lang htdp/isl+

#|
Exercise 359.
Design eval-function*. The function consumes ex, a BSL-fun-expr, and da, a BSL-fun-def* representation of a definitions area.
It produces the result that DrRacket shows if you evaluate ex in the interactions area, assuming the definitions area contains da.

The function works like eval-definition1 from exercise 357.
For an application of some function f, it

1. evaluates the argument;
2. looks up the definition of f in the BSL-fun-def representation of da, which comes with a parameter and a body;
3. substitutes the value of the argument for the function parameter in the function’s body; and
4. evaluates the new expression via recursion.

Like DrRacket, eval-function* signals an error when it encounters a variable or function name without definition in the definitions area. 
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

(define-struct fun-def [name parameter body])
; A BSL-fun-def is a structure:
; (make-fun-def Symbol Symbol BSL-fun-expr)
; interpretation (make-fun-def n p b) represent a function defination
; it's function name is n, p is it's parameter, b is function body

; A BSL-fun-def* is [List-of BSL-fun-def]

; BSL-fun-expr BSL-fun-def*
; It produces the result that DrRacket shows if you evaluate ex in the interactions area,
; assuming the definitions area contains da.
(define (eval-function* ex da) ...)

(check-expect (eval-function* 



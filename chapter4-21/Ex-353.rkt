#lang htdp/isl+

; Exercise 353. Design the numeric? function.
; It determines whether a BSL-var-expr is also a BSL-expr.
; Here we assume that your solution to exercise 345 is the definition for BSL-var-expr without Symbols.

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define ex1 1)
(define ex2 'x)
(define ex3 (make-add 1 1))
(define ex4 (make-mul 2 3))
(define ex5 (make-add 2 'x))
(define ex6 (make-mul 3 'y))
(define ex7 (make-add (make-mul 2 3) 3))
(define ex8 (make-mul (make-add 2 'x) 4))

; A BSL-expr is one of the following:
; - Number
; - Add
; - Mul


(define-struct add [left right])
; An Add is a structure:
; (make-add BSL-expr BSL-expr)
; interpreation (make-add a b) represent sum of a & b

(define-struct mul [left right])
; A Mul is a structure:
; (make-mul BSL-expr BSL-expr)
; interpretation (make-mul a b) represent muliply of a & b

; BSL-var-expr -> Boolean
; determine if ex is a BSL-expr
(define (numeric? ex) #f)

; Add -> Boolean
; determine if ex is a BSL-expr
(define (numeric-add? ex) #f)


; Mul -> Boolean
; determine if ex is a BSL-expr
(define (numeric-mul? ex) #f)


#lang htdp/isl+

; Exercise 347.
; Design eval-expression.
; The function consumes a representation of a BSL expression and computes its value.

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

(define ex1 3)
(define ex2 (make-add 1 1))
(define ex3 (make-mul 3 10))
(define ex4 (make-add (make-mul 1 1) 10))


; BSL-expr -> Number
; computes the value of exp
(define (eval-expression exp)
  (cond [(add? exp) (eval-expression-add exp)]
        [(mul? exp) (eval-expression-mul exp)]
        [else exp]))

(check-expect (eval-expression ex1) 3)
(check-expect (eval-expression ex2) 2)
(check-expect (eval-expression ex3) 30)
(check-expect (eval-expression ex4) 11)

; Add -> Number
; calculate the value of an Add
(define (eval-expression-add exp)
  (+ (eval-expression (add-left exp))
     (eval-expression (add-right exp))))

(check-expect (eval-expression-add ex2) 2)

; Mul -> Number
; calculate the value of a Mul
(define (eval-expression-mul exp)
  (* (eval-expression (mul-left exp))
     (eval-expression (mul-right exp))))

(check-expect (eval-expression-mul ex3) 30)
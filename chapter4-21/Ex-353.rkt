#lang htdp/isl+

; Exercise 353. Design the numeric? function.
; It determines whether a BSL-var-expr is also a BSL-expr.
; Here we assume that your solution to exercise 345 is the definition for BSL-var-expr without Symbols.

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

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

(define ex1 1)
(define ex2 'x)
(define ex3 (make-add 1 1))
(define ex4 (make-mul 2 3))
(define ex5 (make-add 2 'x))
(define ex6 (make-mul 3 'y))
(define ex7 (make-add (make-mul 2 3) 3))
(define ex8 (make-mul (make-add 2 'x) 4))

; BSL-var-expr -> Boolean
; determine if ex is a BSL-expr
(define (numeric? ex)
  (cond [(number? ex) #t]
        [(symbol? ex) #f]
        [(add? ex) (numeric-add? ex)]
        [(mul? ex) (numeric-mul? ex)]))

(check-expect (numeric? ex1) #t)
(check-expect (numeric? ex2) #f)
(check-expect (numeric? ex3) #t)
(check-expect (numeric? ex4) #t)
(check-expect (numeric? ex5) #f)
(check-expect (numeric? ex6) #f)
(check-expect (numeric? ex7) #t)
(check-expect (numeric? ex8) #f)

; Add -> Boolean
; determine if ex is a BSL-expr
(define (numeric-add? ex)
  (and (numeric? (add-left ex))
       (numeric? (add-right ex))))

(check-expect (numeric-add? ex3) #t)
(check-expect (numeric-add? ex5) #f)
(check-expect (numeric-add? ex7) #t)


; Mul -> Boolean
; determine if ex is a BSL-expr
(define (numeric-mul? ex)
  (and (numeric? (mul-left ex))
       (numeric? (mul-right ex))))

(check-expect (numeric-mul? ex4) #t)
(check-expect (numeric-mul? ex6) #f)
(check-expect (numeric-mul? ex8) #f)

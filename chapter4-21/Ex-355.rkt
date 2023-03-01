#lang htdp/isl+

; Exercise 355. Design eval-var-lookup.
; This function has the same signature as eval-variable*:


; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

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
(define ex5 (make-add 2 'x))
(define ex7 (make-add (make-mul 2 3) 3))
(define ex8 (make-mul (make-add 2 'x) 4))
(define ex9 (make-mul (make-add 2 'x) 'y))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define al0 '())
(define al1 '((x 2)))
(define al2 '((y 3)))
(define al3 '((x 3) (y 4)))

(define WRONG "input error")

; BSL-var-expr AL -> Number
; computes the value of exp
(define (eval-var-lookup e da)
  (cond [(add? e) (eval-expression-add e da)]
        [(mul? e) (eval-expression-mul e da)]
        [(symbol? e) (if (false? (assq e da)) (error WRONG) (second (assq e da)))]
        [else e]))

; Add -> Number
; calculate the value of an Add
(define (eval-expression-add exp da)
  (+ (eval-var-lookup (add-left exp) da)
     (eval-var-lookup (add-right exp) da)))

; Mul -> Number
; calculate the value of a Mul
(define (eval-expression-mul exp da)
  (* (eval-var-lookup (mul-left exp) da)
     (eval-var-lookup (mul-right exp) da)))

(check-expect (eval-var-lookup ex1 al0) 1)
(check-expect (eval-var-lookup ex2 al1) 2)
(check-error (eval-var-lookup ex2 al2) WRONG)
(check-expect (eval-var-lookup ex3 al2) 2)
(check-expect (eval-var-lookup ex5 al1) 4)
(check-error (eval-var-lookup ex5 al2) WRONG)
(check-expect (eval-var-lookup ex7 al2) 9)
(check-expect (eval-var-lookup ex8 al3) 20)
(check-expect (eval-var-lookup ex9 al3) 20)
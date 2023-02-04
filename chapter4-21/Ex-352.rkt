#lang htdp/isl+

(define-struct add [left right])
; An Add is a structure:
; (make-add BSL-var-expr BSL-var-expr)
; interpreation (make-add a b) represent sum of a & b

(define-struct mul [left right])
; A Mul is a structure:
; (make-mul BSL-var-expr BSL-var-expr)
; interpretation (make-mul a b) represent muliply of a & b

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define ex1 'x)
(define ex2 (make-add 'x 3))
(define ex3 (make-mul 1/2 (make-mul 'x 3)))
(define ex4 (make-add (make-mul 'x 'x)
                      (make-mul 'y 'y)))

; BSL-var-expr Symbol Number -> BSL-var-expr
; It produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst ex x v)
  (cond [(symbol? ex) ...]
        [(number? ex) ...]
        [(add? ex) ...]
        [(mul? ex) ....]))

(define (


(check-expect (subst ex1 'x 2) 2)
(check-expect (subst ex2 'y 1) ex2)
(check-expect (subst ex2 'x 2) (make-add 2 3))
(check-expect (subst ex3 'x 5) (make-mul 1/2 (make-mul 5 3)))
(check-expect (subst ex4 'x 3) (make-add (make-mul 3 3)
                                         (make-mul 'y 'y)))
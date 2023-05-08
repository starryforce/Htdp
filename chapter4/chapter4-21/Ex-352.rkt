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

(define ex0 20)
(define ex1 'x)
(define ex2 (make-add 'x 3))
(define ex3 (make-mul 1/2 (make-mul 'x 3)))
(define ex4 (make-add (make-mul 'x 'x)
                      (make-mul 'y 'y)))

; BSL-var-expr Symbol Number -> BSL-var-expr
; It produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst ex x v)
  (cond [(number? ex) (subst-number ex x v)]
        [(symbol? ex) (subst-symbol ex x v)]
        [(add? ex) (subst-add ex x v)]
        [(mul? ex) (subst-mul ex x v)]))

(check-expect (subst ex1 'x 2) 2)
(check-expect (subst ex2 'y 1) ex2)
(check-expect (subst ex2 'x 2) (make-add 2 3))
(check-expect (subst ex3 'x 5) (make-mul 1/2 (make-mul 5 3)))
(check-expect (subst ex4 'x 3) (make-add (make-mul 3 3)
                                         (make-mul 'y 'y)))

; Number Symbol Number -> Number
; do nothing to ex
(define (subst-number ex x v) ex)

(check-expect (subst-number 2 'x 4) 2)

; Symbol Symbol Number -> Number
; replace with number if ex match x
(define (subst-symbol ex x v) (if (symbol=? ex x) v ex))

(check-expect (subst-symbol 'x 'x 2) 2)
(check-expect (subst-symbol 'x 'y 3) 'x)

; Add Symbol Number -> Add
; produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst-add ex x v) (make-add (subst (add-left ex)  x v) (subst (add-right ex)  x v)))

(check-expect (subst-add ex2 'x 2) (make-add 2 3))
(check-expect (subst-add ex2 'y 2) ex2)

; Mul Symbol Number -> Mul
; produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst-mul ex x v) (make-mul (subst (mul-left ex)  x v) (subst (mul-right ex) x v)))

(check-expect (subst-mul ex3 'y 1) ex3)
(check-expect (subst-mul ex3 'x 2) (make-mul 1/2 (make-mul 2 3)))



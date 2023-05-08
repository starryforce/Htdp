#lang htdp/isl+

; Exercise 354. Design eval-variable.
; The checked function consumes a BSL-var-expr and determines its value if numeric? yields true for the input.
; Otherwise it signals an error.

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
(define ex9 (make-mul (make-add 2 'x) 'y))

; BSL-var-expr -> Boolean
; determine if ex is a BSL-expr
(define (numeric? ex)
  (cond [(number? ex) #t]
        [(symbol? ex) #f]
        [(add? ex) (numeric-add? ex)]
        [(mul? ex) (numeric-mul? ex)]))

; Add -> Boolean
; determine if ex is a BSL-expr
(define (numeric-add? ex)
  (and (numeric? (add-left ex))
       (numeric? (add-right ex))))

; Mul -> Boolean
; determine if ex is a BSL-expr
(define (numeric-mul? ex)
  (and (numeric? (mul-left ex))
       (numeric? (mul-right ex))))


; BSL-expr -> Number
; computes the value of exp
(define (eval-expression exp)
  (cond [(add? exp) (eval-expression-add exp)]
        [(mul? exp) (eval-expression-mul exp)]
        [else exp]))

; Add -> Number
; calculate the value of an Add
(define (eval-expression-add exp)
  (+ (eval-expression (add-left exp))
     (eval-expression (add-right exp))))

; Mul -> Number
; calculate the value of a Mul
(define (eval-expression-mul exp)
  (* (eval-expression (mul-left exp))
     (eval-expression (mul-right exp))))


(define WRONG "input error")

; BSL-var-expr -> [Maybe Number]
; determines ex's value if numeric? yields true
; otherwise signals an error
(define (eval-variable ex)
  (if (numeric? ex) (eval-expression ex) (error WRONG)))

(check-expect (eval-variable ex1) 1)
(check-error (eval-variable ex2) WRONG)
(check-expect (eval-variable ex3) 2)
(check-expect (eval-variable ex4) 6)
(check-error (eval-variable ex5) WRONG)
(check-error (eval-variable ex6) WRONG)
(check-expect (eval-variable ex7) 9)
(check-error (eval-variable ex8) WRONG)


; BSL-var-expr Symbol Number -> BSL-var-expr
; It produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst ex x v)
  (cond [(number? ex) (subst-number ex x v)]
        [(symbol? ex) (subst-symbol ex x v)]
        [(add? ex) (subst-add ex x v)]
        [(mul? ex) (subst-mul ex x v)]))

; Number Symbol Number -> Number
; do nothing to ex
(define (subst-number ex x v) ex)

; Symbol Symbol Number -> Number
; replace with number if ex match x
(define (subst-symbol ex x v) (if (symbol=? ex x) v ex))

; Add Symbol Number -> Add
; produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst-add ex x v) (make-add (subst (add-left ex)  x v) (subst (add-right ex)  x v)))

; Mul Symbol Number -> Mul
; produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst-mul ex x v) (make-mul (subst (mul-left ex)  x v) (subst (mul-right ex) x v)))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define al0 '())
(define al1 '((y 2)))
(define al2 '((x 2) (y 1)))

; BSL-var-expr [List-of Association] -> [Maybe Number]
; Starting from ex, it iteratively applies subst to all associations in da.
; If numeric? holds for the result, it determines its value;
; otherwise it signals the same error as eval-variable. 
(define (eval-variable* ex da)
  (eval-variable (foldr (lambda (a prev) (subst prev (first a) (second a)))
                        ex
                        da)))

(check-expect (eval-variable* ex1 al0) 1)
(check-error (eval-variable* ex5 al1) WRONG)
(check-expect (eval-variable* ex5 al2) 4)
(check-error (eval-variable* ex8 al0) WRONG)
(check-expect (eval-variable* ex9 al2) 4)
#lang htdp/isl+

#| Exercise 357. 
Design eval-definition1. The function consumes four arguments:
1. a BSL-fun-expr ex;
2. a symbol f, which represents a function name;
3. a symbol x, which represents the functions’ parameter; and
4.a BSL-fun-expr b, which represents the function’s body.

It determines the value of ex. When eval-definition1 encounters an application of f to some argument, it
1. evaluates the argument,
2. substitutes the value of the argument for x in b; and
3. finally evaluates the resulting expression with eval-definition1.
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

(define WRONG "not a valid expression")

; (k (+ 1 1))
(define ex1 (make-fn 'k (make-add 1 1)))

; (* 5 (k (+ 1 1)))
(define ex2 (make-mul 5 (make-fn 'k (make-add 1 1))))

; (* (i 5) (k (+ 1 1)))
(define ex3 (make-mul (make-fn 'i 5) (make-fn 'k (make-add 1 1 ))))

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> [Maybe Number]
; determine the value of ex with all application of function f replaced with b
(define (eval-definition1 ex f x b)
  (cond [(number? ex) ex]
        [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                      (eval-definition1 (add-right ex) f x b))]
        [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                      (eval-definition1 (mul-right ex) f x b))]
        [(fn? ex) (eval-fn ex f x b)]
        [else (error WRONG)]))

(check-expect (eval-definition1 18 'k 'x (make-add 'x 1)) 18)
(check-error (eval-definition1 'x 'k 'x (make-add 'x 1)) WRONG)
(check-expect (eval-definition1 ex1 'k 'x (make-add 'x 1)) 3)
(check-expect (eval-definition1 ex1 'k 'y (make-mul 'y 'y)) 4)
(check-expect (eval-definition1 ex2 'k 'x (make-mul 'x 10)) 100)

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
(define (subst-add ex x v) (make-add (subst (add-left ex)  x v)
                                     (subst (add-right ex)  x v)))

; Mul Symbol Number -> Mul
; produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst-mul ex x v) (make-mul (subst (mul-left ex)  x v)
                                     (subst (mul-right ex) x v)))

; Fn Symbol Symbol BSL-fun-expr -> [Maybe Number]
; determine the value of ex
(define (eval-fn ex f x b)
  (if (symbol=? (fn-name ex) f)
      (local ((define value (eval-definition1 (fn-arg ex) f x b))
              (define plugd (subst b x value)))
        (eval-definition1 plugd f x b))
      (error WRONG)))

(check-error (eval-fn ex1 'j 'x (make-add 'x 1)) WRONG)
(check-expect (eval-fn ex1 'k 'x (make-add 'x 1)) 3)
(check-expect (eval-fn ex1 'k 'y (make-mul 'y 'y)) 4)


#lang htdp/isl+

#|
Exercise 361.
Design eval-all. Like eval-function* from exercise 359,
this function consumes the representation of an expression and a definitions area.
It produces the same value that DrRacket shows if the expression is entered at the prompt
in the interactions area and the definitions area contains the appropriate definitions.
Hint Your eval-all function should process variables in the given expression like eval-var-lookup in exercise 355.
|#

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fn Symbol BSL-fun-expr)

(define ex-1 2)
(define ex-2 'z)

(define-struct add [left right])
; An Add is a structure:
; (make-add BSL-fun-expr BSL-fun-expr)
; interpreation (make-add a b) represent sum of a & b
(define ex-add-1 (make-add 2 3)) ;5
(define ex-add-2 (make-add 2 'k)) ;error
(define ex-add-3 (make-add ex-add-1 1)) ; 6

(define-struct mul [left right])
; A Mul is a structure:
; (make-mul BSL-fun-expr BSL-fun-expr)
; interpretation (make-mul a b) represent muliply of a & b
(define ex-mul-1 (make-mul 2 3)) ;6
(define ex-mul-2 (make-mul ex-add-3 ex-mul-1)) ;36

(define-struct fn [name arg])
; An Fn is a structure:
; (make-fn Symbol BSL-fun-expr)
; interpretation (make-fn n a) represent the application of function n to a
(define ex-fn-1 (make-fn 'a ex-add-3)) ; param 6
(define ex-fn-2 (make-fn 'b ex-mul-2)) ; param 36
(define ex-fn-3 (make-fn 'z 3))
(define ex-fn-4 (make-fn 'a ex-fn-2)) ; param 72

; An BSL-con-def is a list of two items:
;   (cons Symbol (cons Number '())).

(define-struct fun-def [name parameter body])
; A BSL-fun-def is a structure:
; (make-fun-def Symbol Symbol BSL-fun-expr)
; interpretation (make-fun-def n p b) represent a function defination
; it's function name is n, p is it's parameter, b is function body
(define ex-fun-def-1 (make-fun-def 'a 'x (make-add 1 'x)))
(define ex-fun-def-2 (make-fun-def 'b 'y (make-mul 2 'y)))

; An Association is one of:
; - BSL-con-def
; - BSL-fun-def

(define a1 '(close-to-pi 3.14))
(define a2 (make-fun-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r))))
(define a3 (make-fun-def 'volume-of-10-cylinder 'r (make-mul 10 (make-fn 'area-of-circle 'r))))

; A BSL-da-all is [List-of Association].
(define bda (list a1 a2 a3))

(define ERROR_NO_CONST "no such constant definition can be found.")

; BSL-da-all Symbol -> [Maybe BSL-con-def]
; It produces the representation of a constant definition whose name is x, if such a piece of data exists in da;
; otherwise the function signals an error
(define (lookup-con-def da x)
  (cond [(empty? da) (error ERROR_NO_CONST)]
        [else (if (match-const? (first da) x)
                  (first da)
                  (lookup-con-def (rest da) x))]))

(check-expect (lookup-con-def bda 'close-to-pi) a1)
(check-error (lookup-con-def bda 'area-of-circle) ERROR_NO_CONST)
(check-error (lookup-con-def bda 'g) ERROR_NO_CONST)

; Association Symbol -> Boolean
; determine if a is the const defination that x refer
(define (match-const? a x)
  (cond [(fun-def? a) #false]
        [else (symbol=? (first a) x)]))

(check-expect (match-const? a1 'close-to-pi) #true)
(check-expect (match-const? a1 'g) #false)
(check-expect (match-const? a2 'area-of-circle) #false)


; BSL-da-all Symbol -> [Maybe BSL-fun-def]
; It produces the representation of a function definition whose name is f, if such a piece of data exists in da;
; otherwise the function signals an error
(define ERROR_NO_FUN "no such function definition can be found. ")

(define (lookup-fun-def da f)
  (cond [(empty? da) (error ERROR_NO_FUN)]
        [else (if (match-fun? (first da) f) (first da)
                   (lookup-fun-def (rest da) f))]))

(check-expect (lookup-fun-def bda 'area-of-circle) a2)
(check-error (lookup-fun-def bda 'close-to-pi) ERROR_NO_FUN)
(check-error (lookup-fun-def bda 'g) ERROR_NO_FUN)
(check-expect (lookup-fun-def bda 'volume-of-10-cylinder) a3)

; Association Symbol -> Boolean
; deterimine if a is function defination that f refers to
(define (match-fun? a f)
  (cond [(fun-def? a) (symbol=? (fun-def-name a) f)]
        [else #false]))

(check-expect (match-fun? a1 'close-to-pi) #false)
(check-expect (match-fun? a2 'area-of-circle) #true)
(check-expect (match-fun? a2 'g) #false)
(check-expect (match-fun? a3 'volume-of-10-cylinder) #true)




; A BSL-fun-def* is [List-of BSL-fun-def]
(define ex-da-1 '())
(define ex-da-2 (list ex-fun-def-1))
(define ex-da-3 (list ex-fun-def-1 ex-fun-def-2))

(define WRONG "not valid expression")

; BSL-var-expr Symbol Number -> BSL-var-expr
; It produces a BSL-var-expr like ex with all occurrences of x replaced by v. 
(define (subst ex x v)
  (cond [(number? ex) (subst-number ex x v)]
        [(symbol? ex) (subst-symbol ex x v)]
        [(add? ex) (subst-add ex x v)]
        [(mul? ex) (subst-mul ex x v)]
        [(fn? ex) (make-fn (fn-name ex) (subst (fn-arg ex) x v))]
        [else ex]))


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

; Fn BSL-fun-def* -> [Maybe Number]
; calc the value of ex according to the function definations in da.
(define (eval-fn ex da)
  (local ((define match-fn (lookup-fun-def da (fn-name ex)))
          (define param (fun-def-parameter match-fn))
          (define body (fun-def-body match-fn))
          (define value (eval-all (fn-arg ex) da))
          (define plugd (subst body param value)))
    (eval-all plugd da)))


(check-expect (eval-fn ex-fn-1 ex-da-2) 7)
(check-error (eval-fn ex-fn-1 ex-da-1) ERROR_NO_FUN)
(check-expect (eval-fn ex-fn-2 ex-da-3) 72)
(check-error (eval-fn ex-fn-3 ex-da-3) ERROR_NO_FUN)
(check-expect (eval-fn ex-fn-4 ex-da-3) 73)
 

; BSL-fun-expr BSL-da-all -> [Maybe Number]
; determine the value of ex if da cover all definations in ex
(define (eval-all ex da)
  (cond [(number? ex) ex]
        [(symbol? ex) (second (lookup-con-def da ex))]
        [(add? ex) (+ (eval-all (add-left ex) da) (eval-all (add-right ex) da))]
        [(mul? ex) (* (eval-all (mul-left ex) da) (eval-all (mul-right ex) da))]
        [(fn? ex) (eval-fn ex da)]
        [else (error "not a valid expression")]))

(define delta 0.000001)
(check-expect (eval-all 10 bda) 10)
(check-expect (eval-all 'close-to-pi bda) 3.14)
(check-error (eval-all 'g bda) ERROR_NO_CONST)
(check-within (eval-all (make-fn 'area-of-circle 1) bda) #i3.14 delta)
(check-within (eval-all (make-fn 'volume-of-10-cylinder 1) bda) #i31.4 delta)
(check-within (eval-all (make-mul 3 'close-to-pi) bda) #i9.42 delta)
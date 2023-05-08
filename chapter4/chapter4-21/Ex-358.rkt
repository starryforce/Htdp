#lang htdp/isl+

#|
Exercise 358.
Provide a structure type and a data definition for function definitions.
Recall that such a definition has three essential attributes:

1. the function’s name, which is represented with a symbol;
2. the function’s parameter, which is also a name; and
3. the function’s body, which is a variable expression.

We use BSL-fun-def to refer to this class of data.
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



;(define (f x) (+ 3 x))
(define f (make-fun-def 'f 'x (make-add 3 'x)))

;(define (g y) (f (* 2 y)))
(define g (make-fun-def 'g 'y (make-fn 'f (make-mul 2 'y))))

;(define (h v) (+ (f v) (g v)))
(define h (make-fun-def 'h 'v (make-add (make-fn 'f 'v) (make-fn 'g 'v))))


; A BSL-fun-def* is [List-of BSL-fun-def]

(define da-fgh (list f g h))

(define WRONG "not exist")

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-expect (lookup-def da-fgh 'f) f)
(check-error (lookup-def da-fgh 'e) WRONG)
(define (lookup-def da f)
  (cond [(empty? da) (error WRONG)]
        [else (if (symbol=? (fun-def-name (first da)) f)
                  (first da)
                  (lookup-def (rest da) f))]))

#lang htdp/isl+

#| Exercise 360.
Q1 Formulate a data definition for the representation of DrRacket’s definitions area.
Concretely, the data representation should work for a sequence that
freely mixes constant definitions and one-argument function definitions.
Make sure you can represent the definitions area consisting of three definitions at the beginning of this section.
We name this class of data BSL-da-all.
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

; An BSL-con-def is a list of two items:
;   (cons Symbol (cons Number '())).

(define-struct fun-def [name parameter body])
; A BSL-fun-def is a structure:
; (make-fun-def Symbol Symbol BSL-fun-expr)
; interpretation (make-fun-def n p b) represent a function defination
; it's function name is n, p is it's parameter, b is function body

; An Association is one of:
; - BSL-con-def
; - BSL-fun-def

(define a1 '(close-to-pi 3.14))
(define a2 (make-fun-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r))))
(define a3 (make-fun-def 'volume-of-10-cylinder 'r (make-mul 10 (make-fn 'area-of-circle 'r))))

; A BSL-da-all is [List-of Association].
(define bda (list a1 a2 a3))

#|
Q2 Design the function lookup-con-def. It consumes a BSL-da-all da and a symbol x.
It produces the representation of a constant definition whose name is x, if such a piece of data exists in da;
otherwise the function signals an error saying that no such constant definition can be found.
|#

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


#|
Q3 Design the function lookup-fun-def. It consumes a BSL-da-all da and a symbol f.
It produces the representation of a function definition whose name is f, if such a piece of data exists in da;
otherwise the function signals an error saying that no such function definition can be found. 
|#

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

; Association Symbol -> Boolean
; deterimine if a is function defination that f refers to
(define (match-fun? a f)
  (cond [(fun-def? a) (symbol=? (fun-def-name a) f)]
        [else #false]))

(check-expect (match-fun? a1 'close-to-pi) #false)
(check-expect (match-fun? a2 'area-of-circle) #true)
(check-expect (match-fun? a2 'g) #false)
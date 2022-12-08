#lang htdp/isl+

; Bool-expr is one of following:
; - Boolean
; - Xand
; - Xor
; - Xnot

(define-struct xand [left right])
; An Xand is a structure:
; (make-xand BOOL-expr BOOL-expr)
; interpretation (make-xand a b)
; calc and of a b
(define ex1 (make-xand #t #t))
(define ex2 (make-xand #f #t))

(define-struct xor [left right])
; An Xor is a structure:
; (make-xor BOOL-expr BOOL-expr)
; interpretation (make-xor a b)
; calc or of a b
(define ex3 (make-xor #f #f))
(define ex4 (make-xor #f #t))

(define-struct xnot [in])
; An Xnot is a structure:
; (make-xnot BOOL-expr BOOL-expr)
; interpretation (make-xnot i)
; if i is #t,return #f
(define ex5 (make-xnot #f))

; Bool-expr -> Boolean
; computes the bool value of exp
(define (eval-bool-expression exp) #t)

(check-expect (eval-bool-expression #t) #t)
(check-expect (eval-bool-expression #f) #f)
(check-expect (eval-bool-expression ex1) #t)
(check-expect (eval-bool-expression ex2) #f)
(check-expect (eval-bool-expression ex3) #t)
(check-expect (eval-bool-expression ex4) #t)
(check-expect (eval-bool-expression ex5) #t)
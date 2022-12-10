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
; (make-xnot BOOL-expr)
; interpretation (make-xnot i)
; if i is #t,return #f
(define ex5 (make-xnot #f))

(define ex6 (make-xand (make-xor #f #t) #f))
(define ex7 (make-xnot ex6))
(define ex8 (make-xor ex6 ex7))

; Bool-expr -> Boolean
; computes the bool value of exp
(define (eval-bool-expression exp)
  (cond [(xand? exp) (eval-bool-and exp)]
        [(xor? exp) (eval-bool-or exp)]
        [(xnot? exp) (eval-bool-not exp)]
        [else exp]))

; Xand -> Boolean
; determine an Xand exp's result 
(define (eval-bool-and exp)
  (and (eval-bool-expression (xand-left exp))
       (eval-bool-expression (xand-right exp))))

(check-expect (eval-bool-and ex1) #t)
(check-expect (eval-bool-and ex2) #f)
(check-expect (eval-bool-and ex6) #f)

; Xor -> Boolean
; determine an Xor exp's result 
(define (eval-bool-or exp)
  (or (eval-bool-expression (xor-left exp))
      (eval-bool-expression (xor-right exp))))

(check-expect (eval-bool-or ex3) #f)
(check-expect (eval-bool-or ex4) #t)
(check-expect (eval-bool-or ex8) #t)

; Xnot -> Boolean
; determine an Xor exp's result 
(define (eval-bool-not exp)
  (not (eval-bool-expression (xnot-in exp))))

(check-expect (eval-bool-not ex5) #t)
(check-expect (eval-bool-not ex7) #t)

(check-expect (eval-bool-expression #t) #t)
(check-expect (eval-bool-expression #f) #f)
(check-expect (eval-bool-expression ex1) #t)
(check-expect (eval-bool-expression ex2) #f)
(check-expect (eval-bool-expression ex3) #f)
(check-expect (eval-bool-expression ex4) #t)
(check-expect (eval-bool-expression ex5) #t)
(check-expect (eval-bool-expression ex6) #f)
(check-expect (eval-bool-expression ex7) #t)
(check-expect (eval-bool-expression ex8) #t)
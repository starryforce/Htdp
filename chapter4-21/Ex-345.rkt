#lang htdp/isl+


; A BSL-expr is one of the following:
; - Number
; - Add
; - Mul


(define-struct add [left right])
; An Add is a structure:
; (make-add BSL-expr BSL-expr)
; interpreation (make-add a b) represent sum of a & b
(define ex1 (make-add 1 1))

(define-struct mul [left right])
; A Mul is a structure:
; (make-mul BSL-expr BSL-expr)
; interpretation (make-mul a b) represent muliply of a & b
(define ex2 (make-mul 300001 100000))

; Translate the following expressions into data:
; 1. (+ 10 -10)
(make-add 10 -10)

; 2. (+ (* 20 3) 33)
(make-add (make-mul 20 3) 33)

; 3. (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
(make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9)))

; Interpret the following data as expressions:

; 1. (make-add -1 2)
(+ -1 2)
; 2. (make-add (make-mul -2 -3) 33)
(+ (* -2 -3) 33)
; 3. (make-mul (make-add 1 (make-mul 2 3)) 3.14)
(* (+ 1 (* 2 3)) 3.14)
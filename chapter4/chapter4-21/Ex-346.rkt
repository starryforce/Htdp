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



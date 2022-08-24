#lang htdp/bsl

(define-struct vcat [x-coordinate happiness])
; A VCat is a structure:
; (make-vcat Number Number)
; interpretation (make-vcat x h) describes a virtual
; cat displaying at x position, and it's happiness value is h
(define ex1 (make-vcat 0 0))
(define ex2 (make-vcat 0 50))
(define ex3 (make-vcat 0 51))
(define ex4 (make-vcat 0 100))
(define ex5 (make-vcat 20 0))
(define ex6 (make-vcat 20 50))
(define ex7 (make-vcat 20 51))
(define ex8 (make-vcat 20 100))


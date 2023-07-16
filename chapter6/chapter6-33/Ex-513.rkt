#lang htdp/isl+

#| Exercise 513.
Develop a data representation for the same subset of ISL+ that
uses structures instead of lists.
; Also provide data representations for ex1, ex2, and ex3 following your data definition.
|#

(define-struct la [para body])
; A La is a structure:
; (make-la Symbol Lam)
; representation (make-la s l) , s is parameter, l is body
(define-struct app [fun arg])
; An App is a structure:
; (make-app Lam Lam)
; representation (make-app f a),
; f is the function from an application,
; a is the argument from an application

; A Lam is one of: 
; – a Symbol
; – La
; – App
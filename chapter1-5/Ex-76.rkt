#lang htdp/bsl

; Movie is (make-movie String String Number)
(define-struct movie [title producer year])

; Person is (make-person String String String String)
(define-struct person [name hair eyes phone])

; Pet is (make-pet String Number)
(define-struct pet [name number])

; CD is (make-CD String String Number)
(define-struct CD [artist title price])

; Sweater is (make-sweater String Number String)
(define-struct sweater [material size producer])

#lang htdp/bsl

(define-struct movie [title producer year])
; make-movie
; movie-title movie-producer movie-year
; movie?
(make-movie "西虹市首富" "沈腾" "2018")

(define-struct person [name hair eyes phone])
; make-person
; person-name person-hair person-eyes person-phone
; person?
(make-person "张三" "blue" "green" "025-88888888")

(define-struct pet [name number])
; make-pet
; pet-name pet-number
; pet?
(make-pet "mimi" 2)

(define-struct CD [artist title price])
; make-CD
; CD-artist CD-title CD-price
; CD?
(make-CD "周杰伦" "依然范特西" 29.99)

(define-struct sweater [material size producer])
; make sweater
; sweater-material sweater-size sweater-producer
; sweater?
(make-sweater "wool" "xxl" "Gucci")
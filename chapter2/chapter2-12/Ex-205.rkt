#lang htdp/bsl+

(require 2htdp/itunes)

(define date1 (create-date 2012 1 13 14 56 19))
(define date2 (create-date 2002 9 8 17 4 38))

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
(define ass1 (list "a" #true))
(define ass2 (list "b" 1234))
(define ass3 (list "c" "1234"))
(define ass4 (list "d" date1))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date
; 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
(define lass0 '())
(define lass1 (list ass1 ass2 ass3 ass4))
(define lass2 (list ass1))

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
(define llist0 '())
(define llist1 (list lass0 lass1 lass2))
(define llist2 (list lass2))


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

; String LAssoc Any -> Association | Any
; produces the first Association whose first item is equal to key,
; or default if there is no such Association
(define (find-association key aloa default)
  (cond [(empty? aloa) default]
        [else (if (empty? (select-item key aloa))
                  default
                  (select-item key aloa))]))

(check-expect (find-association "" lass0 #false) #false)
(check-expect (find-association "b" lass1 "milk") (list "b" 1234))
(check-expect (find-association "b" lass2 "milk") "milk")
(check-expect (find-association "e" lass1 "egg") "egg")

; String LAssoc -> Association
; produces the first Association whose first item is equal to key
; if there is not, produce '()
(define (select-item key aloa)
  (cond [(empty? aloa) '()]
        [else (if (match-item? key (first aloa))
                  (first aloa)
                  (select-item key (rest aloa)))]))

(check-expect (select-item "" lass0) '())
(check-expect (select-item "b" lass1) (list "b" 1234))
(check-expect (select-item "b" lass2) '())
(check-expect (select-item "e" lass1) '())

; String Association -> Boolean
; determine if key match the ass
(define (match-item? key ass)
  (string=? key (first ass)))

(check-expect (match-item? "a" ass1) #true)
(check-expect (match-item? "b" ass1) #false)
(check-expect (match-item? "z" ass2) #false)



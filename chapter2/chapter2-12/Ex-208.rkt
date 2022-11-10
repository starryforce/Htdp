#lang htdp/bsl+

#|
Exercise 208. Design boolean-attributes.
The function consumes an LLists and produces the Strings that are associated with a Boolean attribute.
Hint Use create-set from exercise 201.
|#

(require 2htdp/itunes)

(define date1 (create-date 2012 1 13 14 56 19))

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
(define ass1 (list "bbc" #true))
(define ass2 (list "cnn" #false))
(define ass3 (list "cctv" 1234))
(define ass4 (list "btv" "btv"))
(define ass5 (list "Total Time" date1))
 
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
(define lass1 (list ass1))
(define lass2 (list ass1 ass2))
(define lass3 (list ass1 ass3))
(define lass4 (list ass4))

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
(define llist0 '())
(define llist1 (list lass0 lass1 lass2))
(define llist2 (list lass2))
(define llist3 (list lass1 lass3))


; LLists -> List-of-strings
; produces the Strings that are associated with a Boolean attribute in allist
(define (main allist) (create-set (boolean-attributes allist)))

(check-expect (main llist0) '())
(check-expect (main llist1) (list "bbc" "cnn"))
(check-expect (main llist2) (list "bbc" "cnn"))
(check-expect (main llist3) (list "bbc"))

; LLists -> List-of-strings
; produces the Strings that are associated with a Boolean attribute in allist
(define (boolean-attributes allist)
  (cond [(empty? allist) '()]
        [else (concat (boolean-attributes-la (first allist))
                      (boolean-attributes (rest allist)))]))

(check-expect (boolean-attributes llist0) '())
(check-expect (boolean-attributes llist1) (list "bbc" "bbc" "cnn"))
(check-expect (boolean-attributes llist2) (list "bbc" "cnn"))
(check-expect (boolean-attributes llist3) (list "bbc" "bbc"))

; LAssoc -> List-of-strings
; produces the Strings that are associated with a Boolean attribute in aloa
(define (boolean-attributes-la aloa)
  (cond [(empty? aloa) '()]
        [else (if (is-boolean? (first aloa))
                  (cons (first (first aloa)) (boolean-attributes-la (rest aloa)))
                  (boolean-attributes-la (rest aloa)))]))

(check-expect (boolean-attributes-la lass0) '())
(check-expect (boolean-attributes-la lass1) (list "bbc"))
(check-expect (boolean-attributes-la lass2) (list "bbc" "cnn"))
(check-expect (boolean-attributes-la lass3) (list "bbc"))
(check-expect (boolean-attributes-la lass4) '())


; Association -> Boolean
; determine if the value associate with key is Boolean
(define (is-boolean? ass)
  (boolean? (second ass)))

(check-expect (is-boolean? ass1) #true)
(check-expect (is-boolean? ass2) #true)
(check-expect (is-boolean? ass3) #false)
(check-expect (is-boolean? ass4) #false)
(check-expect (is-boolean? ass5) #false)

; List-of-anys List-of-anys -> List-of-anys
(define (concat l1 l2)
  (cond [(empty? l1) l2]
        [else (cons (first l1)
                    (concat (rest l1) l2))]))

(check-expect (concat (list 1) (list 2)) (list 1 2))
(check-expect (concat (list 1 2) (list 3 4)) (list 1 2 3 4))

; List-of-strings -> List-of-strings
; construct a list of strings which every String from alos exactly once
(define (create-set alos)
  (cond [(empty? alos) '()]
        [else (cons (first alos)
                    (create-set (remove-s (first alos) (rest alos))))]))

(check-expect (create-set '()) '())
(check-expect (create-set (list "a" "a" "b" "c")) (list "a" "b" "c"))
(check-expect (create-set (list "a" "b" "c")) (list "a" "b" "c"))

; String List-of-strings -> List-of-strings
; remove every string equal to s in alos
(define (remove-s s alos)
  (cond [(empty? alos) '()]
        [else (if (string=? s (first alos))
                  (remove-s s (rest alos))
                  (cons (first alos) (remove-s s (rest alos))))]))

; real world data
; modify the following to use your chosen name
(define ITUNES-LOCATION "C:/Users/StarryForce/OneDrive/itunes.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

(main list-tracks)

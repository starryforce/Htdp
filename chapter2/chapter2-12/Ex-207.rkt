#lang htdp/bsl+

#|
Exercise 207. Design total-time/list,
which consumes an LLists and produces the total amount of play time.
Hint Solve exercise 206 first.

Once you have completed the design, compute the total play time of your iTunes collection.
Compare this result with the time that the total-time function from exercise 200 computes.
Why is there a difference?
|#

(require 2htdp/itunes)

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
(define ass1 (list "a" #true))
(define ass2 (list "Total Time" 1000))
(define ass3 (list "Total Time" 10))
(define ass4 (list "Total Time" 1))
 
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
(define lass4 (list ass1 ass4))

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
(define llist0 '())
(define llist1 (list lass0 lass1 lass2))
(define llist2 (list lass2))
(define llist3 (list lass2 lass3))
(define llist4 (list lass2 lass4))

; LLists -> Number
; produces the total amount of play time in allist
(define (total-time/list allist)
  (cond [(empty? allist) 0]
        [else (+ (select-time (first allist))
                 (total-time/list (rest allist)))]))

(check-expect (total-time/list llist0) 0)
(check-expect (total-time/list llist1) 1000)
(check-expect (total-time/list llist2) 1000)
(check-expect (total-time/list llist3) 1010)
(check-expect (total-time/list llist4) 1001)


; LAssoc -> Number
; find the first "Total Time" in aloa
(define (select-time aloa)
  (second (find-association "Total Time" aloa (list "default" 0))))

(check-expect (select-time lass0) 0)
(check-expect (select-time lass1) 0)
(check-expect (select-time lass2) 1000)
(check-expect (select-time lass3) 10)
(check-expect (select-time lass4) 1)


; String LAssoc Any -> Association | Any
; produces the first Association whose first item is equal to key,
; or default if there is no such Association
(define (find-association key aloa default)
  (cond [(empty? aloa) default]
        [else (if (empty? (select-item key aloa))
                  default
                  (select-item key aloa))]))

; String LAssoc -> Association
; produces the first Association whose first item is equal to key
; if there is not, produce '()
(define (select-item key aloa)
  (cond [(empty? aloa) '()]
        [else (if (match-item? key (first aloa))
                  (first aloa)
                  (select-item key (rest aloa)))]))

; String Association -> Boolean
; determine if key match the ass
(define (match-item? key ass)
  (string=? key (first ass)))

; real world data
; modify the following to use your chosen name
(define ITUNES-LOCATION "C:/Users/StarryForce/OneDrive/itunes2.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; 446092
(total-time/list list-tracks)


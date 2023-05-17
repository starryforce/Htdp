#lang htdp/isl+

#| Exercise 404.
Design the function andmap2.
It consumes a function f from two values to Boolean and two equally long lists.
Its result is also a Boolean. Specifically,
it applies f to pairs of corresponding values from the two lists,
and if f always produces #true, andmap2 produces #true, too.
Otherwise, andmap2 produces #false.
In short, andmap2 is like andmap but for two lists.
|#

; [Any Any -> Boolean] [List-of Any] [List-of Any] -> Boolean
; if the corresponding values in l1 & l2 applies to f,
; every pairs produces #t, the function produce #t,
; otherwise produce #f
; constraint the length of l1 & l2 is same
(define (andmap2 f l1 l2)
  (cond [(empty? l1) #t]
        [else (and (f (first l1) (first l2))
                   (andmap2 f (rest l1) (rest l2)))]))

(check-expect (andmap2 (lambda (a b) (> (+ a b) 4)) '(1 2 3) '(4 5 6)) #t)
(check-expect (andmap2 (lambda (a b) (< (+ a b) 4)) '(1 2) '(2 5)) #f)

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define school-db
  (make-db school-schema
           school-content))

(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))
(define presence-content
  `((#true  "presence")
    (#false "absence")))
(define presence-db
  (make-db presence-schema
           presence-content))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
 
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check (make-db school-schema
                                        presence-content)) #false)
 
(define (integrity-check db)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define width   (length schema))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap (lambda (s c) [(second s) c])
                         schema
                         row))))
    (andmap row-integrity-check content)))

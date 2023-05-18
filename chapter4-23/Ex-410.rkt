#lang htdp/isl+

#| Exercise 410.
Design the function db-union,
which consumes two databases with the exact same schema and
produces a new database with this schema and the joint content of both.
The function must eliminate rows with the exact same content.

Assume that the schemas agree on the predicates for each column.
|#

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
(define school-content1
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define school-content2
  `(("Eko" 29 #true)
    ("Frank" 19 #false)
    ("Dave"  32 #false)))
(define school-db1
  (make-db school-schema
           school-content1))
(define school-db2
  (make-db school-schema
           school-content2))

; DB DB -> DB
; produces a new database with this schema and the joint content of both db1 & db2
; contrait: the schemas agree on the predicates for each column
(define (db-union db1 db2)
  (local ((define content1 (db-content db1))
          (define content2 (db-content db2))
          ; Row -> Boolean
          ; determine if row is different from every row in c1
          (define (unique? row)
            (andmap (lambda (r) (not (equal? row r))) content1))
          ; Content Content -> Content
          (define (merge c1 c2)
            (append c1
                    (filter unique? c2))))
    (make-db (db-schema db1)
             (merge content1 content2 ))))

(check-expect (db-content (db-union school-db1 school-db2))
              `(("Alice" 35 #true)
                ("Bob"   25 #false)
                ("Carol" 30 #true)
                ("Dave"  32 #false)
                ("Eko" 29 #true)
                ("Frank" 19 #false)))


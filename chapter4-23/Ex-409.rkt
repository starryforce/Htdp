#lang htdp/isl+

#| Exercise 409.
Design reorder. The function consumes a database db and list lol of Labels.
It produces a database like db but with its columns reordered according to lol.
Hint Read up on list-ref.

At first assume that lol consists exactly of the labels of db’s columns.
Once you have completed the design,
study what has to be changed if lol contains fewer labels than there are columns and strings that are not labels of a column in db.
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
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define school-db
  (make-db school-schema
           school-content))

; DB [List-of Label] -> DB
; produces a database like db but with its columns reordered according to lol.
(define (reorder db lol)
  (local (; constant
          (define schema (db-schema db))
          (define content (db-content db))
          (define names (map first schema))
          
          (define (queryorder labels names)
            (map (lambda (label) (findindex label names)) labels))
          (define indexes (queryorder lol names))
          
          ; Schema [List-of Number] -> Schema
          (define (reorder-schema sc order)
            (map (lambda (ith) (list-ref sc ith)) order))
          ; Content [List-of Number] -> Content
          (define (reorder-content c order)
            (local (; Row -> Row
                    ; reorder one row according to order
                    (define (fn row)
                      (map (lambda (ith) (list-ref row ith)) order)))
              (map fn c))))
    (if (not (= (length schema) (length lol)))
        (error "length not equal")
        (make-db (reorder-schema schema indexes)
                 (reorder-content content indexes)))))

(check-expect (map first (db-schema
                          (reorder school-db '("Age" "Present" "Name"))))
              '("Age" "Present" "Name"))
(check-expect (db-content
               (reorder school-db '("Age" "Present" "Name")))
              `((35 #true  "Alice" )
                (25 #false "Bob"   )
                (30 #true  "Carol")
                (32 #false "Dave")))

(check-expect (map first (db-schema
                          (reorder school-db '("Present" "Age" "Name"))))
              '("Present" "Age" "Name"))
(check-expect (db-content
               (reorder school-db '("Present" "Age" "Name")))
              `((#true  35 "Alice" )
                (#false 25 "Bob"   )
                (#true  30 "Carol")
                (#false 32 "Dave")))
(check-error (reorder school-db '("Present" "Age")) "length not equal")

; Any [List-of Any] -> Number
; find the index of target in l
(define (findindex target aloa)
  (cond [(empty? aloa) (error "not exsit")]
        [else (if (equal? (first aloa) target)
                  0
                  (add1 (findindex target (rest aloa))))]))


(check-expect (findindex 0 '(3 2 1 0)) 3)
(check-expect (findindex 1 '(3 2 1 0)) 2)
(check-expect (findindex 2 '(3 2 1 0)) 1)
(check-expect (findindex 3 '(3 2 1 0)) 0)
(check-error (findindex 4 '(3 2 1 0)) "not exsit")
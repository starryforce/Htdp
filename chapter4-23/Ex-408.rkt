#lang htdp/isl+

#| Exercise 408.
Design the function select.
It consumes a database, a list of labels, and a predicate on rows.
The result is a list of rows that satisfy the given predicate,
projected down to the given set of labels. 
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

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))

; DB [List-of Label] [Row -> Boolean] -> [List-of Row]
; get a list of rows that satisfy the given predicate,
; projected down to the given set of labels. 
(define (select db labels condition)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define filtered_content (filter condition content))
          (define filtered_db (make-db schema filtered_content)))
    (db-content (project filtered_db labels))))

(check-expect (select school-db '("Name" "Age") (lambda (row) (> (second row) 30)))
              '(("Alice" 35) ("Dave"  32)))
(check-expect (select school-db '("Name" "Present") (lambda (row) (string=? (first row) "Bob")))
              '(("Bob" #false)))
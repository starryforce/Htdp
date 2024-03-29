#lang htdp/isl+

#| Exercise 406.
The row-project function recomputes the labels for every row of the database’s content.
Does the result differ from function call to function call? If not, hoist the expression.
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

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 
(define projected-db
  (make-db projected-schema projected-content))

(define (project.v3 db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define fields (map first schema))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (local (; Cell Label -> Row
                    (define (fn cell name base)
                      (if (member? name labels)
                          (cons cell base)
                          base)))
              (foldr fn '() row fields))))

    (make-db (filter keep? schema)
             (map row-project content))))

(check-expect
  (db-content (project.v3 school-db '("Name" "Present")))
  projected-content)
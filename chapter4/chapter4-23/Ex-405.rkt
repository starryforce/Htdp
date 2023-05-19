#lang htdp/isl+

#| Exercise 405.
Design the function row-filter.
Construct examples for row-filter from the examples for project.

Assumption The given database passes an integrity check,
meaning each row is as long as the schema and thus its list of names.
|#
(define-struct db [schema content])
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

(define labels '("Name" "Present"))

; Row [List-of Label] -> Row
; retains those cells whose corresponding element 
; in names is also in labels
(define (row-filter row names)
  (cond [(empty? row) '()]
        [else (if (member? (first names) labels)
                  (cons (first row) (row-filter (rest row) (rest names)))
                   (row-filter (rest row) (rest names)))]))

(check-expect (row-filter '("Alice" 35 #true) '("Name" "Age" "Present")) '("Alice" #true))
(check-expect (row-filter '("Bob" 25 #false) '("Name" "Age" "Present")) '("Bob" #false))
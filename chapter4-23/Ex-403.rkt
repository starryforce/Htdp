#lang htdp/isl+

#| Exercise 403.
A Spec combines a Label and a Predicate into a list.
While acceptable, this choice violates our guideline of using a structure type for a fixed number of pieces of information.

Here is an alternative data representation:
(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)
Use this alternative definition to represent the databases from figure 137. 
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

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)


(define school-schema
  (list (make-spec "Name"     string?)
        (make-spec "Age"      integer?)
        (make-spec "Present"  boolean?)))
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define school-db
  (make-db school-schema
           school-content))


(define presence-schema
  (list (make-spec "Present"     boolean?)
        (make-spec "Description" string?)))
(define presence-content
  `((#true  "presence")
    (#false "absence")))
(define presence-db
  (make-db presence-schema
           presence-content))
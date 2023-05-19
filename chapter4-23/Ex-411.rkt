#lang htdp/isl+

#| Exercise 411.
Design join, a function that consumes two databases: db-1 and db-2.
The schema of db-2 starts with the exact same Spec that the schema of db-1 ends in.
The function creates a database from db-1 by replacing the last cell in each row with the translation of the cell in db-2.
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
(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))
(define presence-schema-mul
  `(("Present"     ,boolean?)
    ("Description" ,string?)
    ("Note" ,string?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define presence-content
  `((#true  "presence")
    (#false "absence")))
(define presence-content-mul
  `((#true  "presence" "attend")
    (#false "absence" "not attend")))

(define school-db
  (make-db school-schema
           school-content))
(define presence-db
  (make-db presence-schema
           presence-content))
(define presence-db-mul
  (make-db presence-schema-mul
           presence-content-mul))

; DB DB -> DB
; creates a database from db-1 by replacing
; the last cell in each row with the translation of the cell in db-2.
; constraint: The schema of db-2 starts with
; the exact same Spec that the schema of db-1 ends in.
(define (join db-1 db-2)
  (make-db (joinlink (db-schema db-1) (db-schema db-2))
           (translate (db-content db-1) (db-content db-2))))

(check-expect (map first
                   (db-schema (join school-db presence-db)))
              `("Name" "Age" "Description"))
(check-expect (db-content (join school-db presence-db))
              `(("Alice" 35 "presence")
                ("Bob"   25 "absence")
                ("Carol" 30 "presence")
                ("Dave"  32 "absence")))
(check-expect (map first
                   (db-schema (join school-db presence-db-mul)))
              `("Name" "Age" "Description" "Note"))
(check-expect (db-content (join school-db presence-db-mul))
              `(("Alice" 35 "presence" "attend")
                ("Bob"   25 "absence" "not attend")
                ("Carol" 30 "presence" "attend")
                ("Dave"  32 "absence" "not attend")))

(define ERROR_NOT_FOUND "can't find relate row")
; Content Content -> Content
(define (translate c1 c2)
  (local (; Row Content -> Row
          ; extract the row match r in c
          (define (extractrow r c)
            (cond [(empty? c) (error ERROR_NOT_FOUND)]
                  [else (if (equal? (last r) (first (first c)))
                            (first c)
                            (extractrow r (rest c)))]))
          ; Row -> Row
          ; attach extra info to r from c2
          (define (match r)
            (joinlink r (extractrow r c2))))
    (map match c1)))

(check-expect (translate school-content presence-content)
              `(("Alice" 35 "presence")
                ("Bob"   25 "absence")
                ("Carol" 30 "presence")
                ("Dave"  32 "absence")))

(check-expect (translate school-content presence-content-mul)
              `(("Alice" 35 "presence" "attend")
                ("Bob"   25 "absence" "not attend")
                ("Carol" 30 "presence" "attend")
                ("Dave"  32 "absence" "not attend")))

(define ERROR_SHORT "list is too short")
(define ERROR_MISS_MATCH "lists are not match")
; [List-of Any] [List-of Any] -> [List-of Any]
; connect l1 & l2 into a single list
; constraint the last item of l1 & the first item of l2 is same.
(define (joinlink l1 l2)
  (cond [(or (< (length l1) 2) (< (length l2) 2)) (error ERROR_SHORT)]
        [(not (equal? (last l1) (first l2))) (error ERROR_MISS_MATCH)]
        [else (append (reverse (rest (reverse l1))) (rest l2))]
        ))

(check-expect (joinlink '(1 2) '(2 3)) '(1 3))
(check-error (joinlink '(3 4) '(2 4)) ERROR_MISS_MATCH)

(define ERROR_EMPTY "list should not be empty")
; [List-of Any] -> Any
; extracts the last item of l.
(define (last l)
  (cond [(empty? l) (error ERROR_EMPTY)]
        [else (first (reverse l))]))

(check-error (last '()) ERROR_EMPTY)
(check-expect (last '(1 2)) 2)
(check-expect (last '(1))1)
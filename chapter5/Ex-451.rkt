#lang htdp/isl+

#|
Design find-linear.
The function consumes a monotonically increasing table and
finds the smallest index for a root of the table.
Use the structural recipe for N, proceeding from 0 through 1, 2, and
so on to the array-length of the given table.
This kind of root-finding process is often called a linear search.
|#

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers


(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 10 (lambda (i) (- i 4))))
(define table2 (make-table 1024 (lambda (i) (- i 1023))))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> N
; finds the smallest index for a root of t
(define (find-linear t)
  (local (; N -> N
          (define (fn i)
            (if (= (table-ref t i) 0)
                i
                (fn (add1 i)))))
    (fn 0)))

(check-expect (find-linear table1) 4)

; Table -> N
; finds the smallest index for the root of t
(define (find-binary t) 0)

(check-expect (find-binary table1) 4)
(check-expect (find-binary table2) 1023)
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
(check-expect (find-linear table2) 1023)

#|
Design find-binary, which also finds the smallest index for the root of a monotonically increasing table
but uses generative recursion to do so.
Like ordinary binary search, the algorithm narrows an interval down to the smallest possible size and then chooses the index.
Don’t forget to formulate a termination argument.

Hint
The key problem is that a table index is a natural number, not a plain number.
Hence the interval boundary arguments for find must be natural numbers.
Consider how this observation changes
(1) the nature of trivially solvable problem instances,
(2) the midpoint computation,
(3) and the decision as to which interval to generate next.
To make this concrete, imagine a table with 1024 slots and the root at 1023.
How many calls to find are needed in find-linear and find-binary, respectively? 
|#

; Table -> N
; assume (1) t is a monotonically increasing table
; (2) (<= (table-ref t i)(table-ref t (add1 i)))
; generative finds the smallest index for the root of t by
; divides inteval in half, the root is in one of two halves 
(define (find-binary t)
  (local ((define length (table-length t))
          (define (find-index left right)
            (local (; calc the abs of ith number
                    ; N -> Number
                    (define (offset index)
                      (abs (table-ref t index)))
                    (define mid (floor (/ (+ left right) 2)))
                    (define offset-m (offset mid))
                    (define offset-sub1 (offset (sub1 mid)))
                    (define offset-add1 (offset (add1 mid))))
              (cond [(= left right) left]
                    [(= (add1 left) right)
                     (if (<= (offset left) (offset right)) left right)]
                    [else (if (<= offset-sub1 offset-add1)
                              (find-index left mid)
                              (find-index mid right))]))))
    (find-index 0 (sub1 length))))

; 1. (= left right) only 1 item, so the answer is left
;    (= (add1 left) right) only 2 items,
;    so the answer is the index which offset is smaller.
; 2. as 1. says.
; 3. midpoint (floor (/ (+ left right) 2))

(check-expect (find-binary table1) 4)
(check-expect (find-binary table2) 1023)

; linear 1023 times
; binary 10 times

; left: 0 511 767 895 991 1007 1015 1019 1021 1022

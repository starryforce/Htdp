#lang htdp/isl+

(define ERROR_START_MIN "start should greater than 0")
(define ERROR_END_MIN "end should greater than 0")
(define ERROR_START_MAX "start should less than or equal to the length of list")
(define ERROR_END_MAX "end should less than or equal to the length of list")


(define ex (list 1 2 3 4))

; [List-of Item] Number Number -> [List-of Item]
; get the sub string from l, start to end (end not included)
; constraint:  both start and end should greater than 0
; start should smaller than end
; both start and end should than less or equal to length of l
(define (slice l start end)
  (cond
    ; when start not equal to 0, remove one item from head every time, and transform
    ; to slice from the new removed item;
    [(> start 0) (slice (rest l) (sub1 start) (sub1 end))]
    ; at this point, start should be 0, and end is the length of the result,
    ; this time, remove one item from tail until the length of l is equal to end
    [(> (length l) end) (slice (reverse (rest (reverse l))) start end)]
    [else l]))

(check-expect (slice ex 0 1) (list 1))
(check-expect (slice ex 0 2) (list 1 2))
(check-expect (slice ex 1 2) (list 2))
(check-expect (slice ex 0 4) ex)
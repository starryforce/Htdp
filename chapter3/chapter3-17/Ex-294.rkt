#lang htdp/isl+

(define ex (list 5 4 3 2 1))

; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; [X] X [List-of X]
; -> [[Maybe N] -> Boolean]
; produces a function that determines whether
; the i-th item of l is the first target in l
(define (is-index? target l)
  (lambda (i)
    (cond [(false? i) (not (member? l target))]
    [else
     (local ((define (slice l start end)
               (cond
                 ; when start not equal to 0, remove one item from head every time, and transform
                 ; to slice from the new removed item;
                 [(> start 0) (slice (rest l) (sub1 start) (sub1 end))]
                 ; at this point, start should be 0, and end is the length of the result,
                 ; this time, remove one item from tail until the length of l is equal to end
                 [(> (length l) end) (slice (reverse (rest (reverse l))) start end)]
                 [else l])))
       (and (equal? (list-ref l i) target)
            (not-include? (slice l 0 i) target))
       )])))



; [List-of Item] Item -> Boolean
; determine if x is one item of l
(define (not-include? l x)
  (not (member? x l)))

(check-expect (not-include? (list 1 2 3) #false) #true)
(check-expect (not-include? (list 1 2 3) 1 ) #false)

(check-satisfied (index 4 ex) (is-index? 4 ex))

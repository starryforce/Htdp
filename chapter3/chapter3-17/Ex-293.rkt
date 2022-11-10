#lang htdp/isl+

(define ex (list 0 2 4 1 3 2 9))
(define ex1 (list 0 2 3 2 9 3 2 9))

; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

(check-expect (find 3 ex) (list 3 2 9))
(check-expect (find 2 ex) (list 2 4 1 3 2 9))


; [X] X [List-of X] ->
; [[Maybe [List-of X]] -> Boolean]
; produces a function that determines whether
; some list match the requirement of find.
; todo: split l into to parts, the second part should have the
; same length with sl, every item i each list should be same.
; and the rest part should not contain x
(define (found? x l)
  (lambda (sl)
    (local ((define (slice l start end)
              (cond
                ; when start not equal to 0, remove one item from head every time, and transform
                ; to slice from the new removed item;
                [(> start 0) (slice (rest l) (sub1 start) (sub1 end))]
                ; at this point, start should be 0, and end is the length of the result,
                ; this time, remove one item from tail until the length of l is equal to end
                [(> (length l) end) (slice (reverse (rest (reverse l))) start end)]
                [else l]))
            )
      (cond
        ; - #false
        [(false? sl) (not (member? x l))]
        ; - [List-of X]
        [else (and
               (is-same-list? (reverse sl) (slice (reverse l) 0 (length sl)))
               (not (member? x (slice l 0 (- (length l) (length sl)))))
                             (equal? (first sl) x)
                             (andmap (lambda (i) (member? i l)) sl))]))))

(check-expect [(found? 3 ex) (list 3 2 9)] #true)
(check-expect [(found? 3 ex) (list 3 1 9)] #false)
(check-expect [(found? 7 ex) #false] #true)
(check-expect [(found? 2 ex) (list 2 4 1 3 2 9)] #true)
(check-expect [(found? 3 ex1) (list 3 2 9)] #false)
(check-expect [(found? 3 ex1) (list 3 2 9 3 2 9)] #true)

(check-satisfied (find 3 ex) (found? 3 ex))
(check-satisfied (find 2 ex) (found? 2 ex))
(check-satisfied (find 3 ex1) (found? 3 ex1))
(check-satisfied (find 2 ex1) (found? 2 ex1))


; [List-of Item] [List-of Item] -> Boolean
; determine if lx and ly have same item and same order
(define (is-same-list? lx ly)
  (cond [(= (length lx) (length ly)) (foldl (lambda (x y base) (and base (equal? x y))) #true lx ly)]
        [else #false]))


(check-expect (is-same-list? (list 1 2 3) (list 1 2 3)) #true)
(check-expect (is-same-list? (list 1 2 3) (list 3 2)) #false)
(check-expect (is-same-list? '() '()) #true)



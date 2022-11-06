#lang htdp/isl+

(define ex (list 0 2 4 1 3 2 9))

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
(define (found? x l)
  (lambda (sl)
    #true))

(check-expect [(found? 3 ex) (list 3 2 9)] #true)
(check-expect [(found? 3 ex) (list 3 1 9)] #false)
(check-expect [(found? 7 ex) #false] #true)
(check-expect [(found? 2 ex) (list 2 4 1 3 2 9)] #true)
#lang htdp/isl

(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))

; Posn Polygon -> Polygon
; add the first item of p to the end of p
(define (add-at-end pos p)
  (cond [(empty? p) (list pos)]
        [else (cons (first p)
                    (add-at-end pos (rest p)))]))

(check-expect (add-at-end (make-posn 1 2) triangle-p)
              (list (make-posn 20 10)
                    (make-posn 20 20)
                    (make-posn 30 20)
                    (make-posn 1 2) ))

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
(define (build-l*st n f)
  (cond [(= n 1) (list (f 0))]
        [else (add-at-end (f (sub1 n)) (build-l*st (sub1 n) f))]))

(check-expect (build-l*st 3 add1) (list 1 2 3))

(define b
  (local ((define a 1))
    ; - IN -
    a))
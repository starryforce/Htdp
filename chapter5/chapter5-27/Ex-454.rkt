#lang htdp/isl+

#| Exercise 454.
Design create-matrix.
The function consumes a number n and a list of n2 numbers.
It produces an image matrix, for example:

Make up a second example.
|#

; N [List-of N] -> [List-of [List-of N]]
; produces an image matrix
; constrait: (length alon) equals to n
(define (create-matrix n alon)
  (cond [(empty? alon) '()]
        [else (cons (first-row n alon)
                    (create-matrix n (remove-first-row n alon)))]))

(check-expect
 (create-matrix 2 (list 1 2 3 4))
 (list (list 1 2)
       (list 3 4)))

(check-expect
 (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
 (list (list 1 2 3)
       (list 4 5 6)
       (list 7 8 9)))

; N [List-of N] -> [List-of N]
; retrieves the first n numbers
(define (first-row n alon)
  (cond [(= n 0) '()]
        [else (cons (first alon)
                    (first-row (sub1 n) (rest alon)))]))

(check-expect (first-row 2 (list 1 2 3 4)) (list 1 2))

; N [List-of N] -> [List-of N]
; remove the first n numbers in alon
(define (remove-first-row n alon)
  (cond [(= n 0) alon]
        [else (remove-first-row (sub1 n) (rest alon))]))

(check-expect (remove-first-row 2 (list 1 2 3 4)) (list 3 4))
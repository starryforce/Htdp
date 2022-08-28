#lang htdp/bsl

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
 
(check-expect (transpose mat1) tam1)
 
(define (transpose lln)
  
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; Matrix -> List-of-numbers;
; consumes a matrix and produces the first column as a list of numbers
(define (first* lln)
  (cond [(empty? lln) '()]
        [else  (cons (first (first lln)) (first* (rest lln)))]))

(check-expect (first* mat1) wor1)

; Matrix -> Matrix
; consumes a matrix and removes the first column
(define (rest* lln)
  (cond [(empty? lln) '()]
        [else (cons (rest (first lln)) (rest* (rest lln)))]))

(check-expect (rest* mat1) (cons (cons 12 '()) (cons (cons 22 '()) '())))



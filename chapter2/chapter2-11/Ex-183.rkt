#lang htdp/bsl+

(define ex1 (cons "a" (list 0 #false)))
(define ex2 (list (cons 1 (cons 13 '()))))
(define ex3 (cons (list 1 (list 13 '())) '()))
(define ex4 (list '() '() (cons 1 '())))
(define ex5 (cons "a" (cons (list 1) (list #false '()))))

(check-expect ex1 (cons "a" (cons 0 (cons #false '()))))
(check-expect ex2 (cons (cons 1 (cons 13 '())) '()))
(check-expect ex3 (cons (cons 1 (cons (cons 13 (cons '() '())) '())) '()))
(check-expect ex4 (cons '() (cons '() (cons (cons 1 '()) '()))))
(check-expect ex5 (cons "a" (cons (cons 1 '()) (cons #false (cons '() '())))))

(check-expect ex1 (list "a" 0 #false))
(check-expect ex2 (list (list 1 13)))
(check-expect ex3 (list (list 1 (list 13 '()))))
(check-expect ex4 (list '() '() (list 1)))
(check-expect ex5 (list "a" (list 1) #false '()))
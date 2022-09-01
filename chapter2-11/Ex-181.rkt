#lang htdp/bsl+

(define ex1 (cons "a" (cons "b" (cons "c" (cons "d" '())))))
(define ex2 (cons (cons 1 (cons 2 '())) '()))
(define ex3 (cons "a" (cons (cons 1 '()) (cons #false '()))))
(define ex4 (cons (cons "a" (cons 2 '())) (cons "hello" '())))

(check-expect ex1 (list "a" "b" "c" "d"))
(check-expect ex2 (list (list 1 2)))
(check-expect ex3 (list "a" (list 1) #false))
(check-expect ex4 (list (list "a" 2) "hello" ))

(define ex5 (cons (cons 1 (cons 2 '()))
                  (cons (cons 2 '())
                        '())))
(check-expect ex5 (list (list 1 2) (list 2)))
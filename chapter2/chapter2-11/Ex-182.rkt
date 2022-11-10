#lang htdp/bsl+

(define ex1 (list 0 1 2 3 4 5))
(define ex2 (list (list "he" 0) (list "it" 1) (list "lui" 14)))
(define ex3 (list 1 (list 1 2) (list 1 2 3)))




(check-expect ex1 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))
(check-expect ex2 (cons (cons "he" (cons 0 '())) (cons (cons "it" (cons 1 '())) (cons (cons "lui" (cons 14 '())) '()))))
(check-expect ex3 (cons 1  (cons (cons 1 (cons 2 '())) (cons (cons 1 (cons 2 (cons 3 '()))) '()))))

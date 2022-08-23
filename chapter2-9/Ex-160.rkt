#lang htdp/bsl


(define ex1 (cons 1 (cons 1 '())))
(define ex2 (cons 2  '()))
(define ex3 '())

; Son
(define es '())
 
; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))

; Number Son.L -> Son.L
; add x to the set s
(define (set+.L x s) (cons x s))

(check-expect (set+.L 1 ex1) (cons 1 ex1))
(check-expect (set+.L 2 ex2) (cons 2 ex2))
(check-expect (set+.L 3 ex3) (cons 3 '()))



; Number Son.R -> Son.R
; add x to the set s, but keep numbers in s unique.
(define (set+.R x s)
  (cond [(empty? s) '()]
        [else (cons x (if (= (first s) x)
                   (set+.R x (rest s))
                   (cons (first s) (set+.R x (rest s)))))]))

(check-expect (in? 1 (set+.R 1 ex1)) #true)
(check-expect (in? 2 (set+.R 2 ex2)) #true)
(check-expect (in? 1 (set+.R 1 ex2)) #true)
(check-expect (in? 1 (set+.R 1 ex3)) #true)

#lang htdp/isl+

#| Exercise 395.
Design take.
It consumes a list l and a natural number n.
It produces the first n items from l or all of l if it is too short.

Design drop.
It consumes a list l and a natural number n.
Its result is l with the first n items removed or just ’() if l is too short. 
|#

; N is one of: 
; – 0
; – (add1 N)

; [List-of Any] N ->[List-of Any]
; produces the first n items in l
(define (take l n)
  (cond [(empty? l) '()]
        [(= n 0) '()]
        [(> n 0)
         (cons (first l) (take (rest l) (sub1 n)))]))

; (take '(1 2 3) 2) == '(1 2)
; (take l (sub1 n)) => (take '(1 2 3) 1) === '(1)
; (take (rest l) n)) => (take '(2 3) 2) ==='(2 3)
; (take (rest l) (sub1 n)) => (take '(2 3) 1) => '(2)

(check-expect (take '() 0) '())
(check-expect (take '() 1) '())
(check-expect (take '(1 2 3) 0) '())
(check-expect (take '(1 2 3) 2) '(1 2))
(check-expect (take '("apple" "bad" 3 4 5 ) 2) '("apple" "bad"))

; [List-of Any] N -> [List-of Any]
; drops the first n items in l
(define (drop l n)
  (cond [(empty? l) '()]
        [(= n 0) l]
        [(> n 0) (drop (rest l) (sub1 n))]))


; (drop (rest l) n) => (drop '(2 3) 2)) == '()
; (drop (rest l) (sub1 n)) => (drop '(2 3) 1) == '(3) 
; (drop l (sub1 n) => (drop '(1 2 3) 1) == '(2 3)

(check-expect (drop '() 0) '())
(check-expect (drop '() 1) '())
(check-expect (drop '(1) 0) '(1))
(check-expect (drop '(1 2 3) 2) '(3))
(check-expect (drop '(1 9 2 8 3 7) 4) '(3 7))
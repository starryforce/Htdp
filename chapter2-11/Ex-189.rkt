#lang htdp/bsl+

; A List-of-sorted-numbers is one of:
; - '()
; - (cons Number List-of-sorted-numbers)
; constraint every number is larger than or equal to the numbers after it

(define ex1 '())
(define ex2 (list 5 4 3 2 1))

; Number List-of-sorted-numbers -> Boolean;
; determines if n exist in alosn
(define (search-sorted n alosn)
  (cond [(empty? alosn) #false]
        [else (cond
                [(= (first alosn) n) #true]
                [(< (first alosn) n) #false]
                [else  (search-sorted n (rest alosn))])]))

(check-expect (search-sorted 1 ex1) #false)
(check-expect (search-sorted 3 ex2) #true)
(check-expect (search-sorted 10 ex2) #false)

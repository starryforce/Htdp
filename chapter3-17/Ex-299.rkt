#lang htdp/isl+

; A NumberSet is a function:
; [Number -> Boolean]
; interpretation if ns is a number set and n is a number
; produces #true if n belongs to s.

; NumberSet Number -> Boolean
(define (contains? ns n) (ns n))


(define my-odd (lambda (n) (and (integer? n)
                                (= (modulo n 2) 1))))


(define my-even (lambda (n) (and (integer? n)
                                (= (modulo n 2) 0))))
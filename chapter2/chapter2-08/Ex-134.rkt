#lang htdp/bsl

; A List-of-Strings is one of:
; - '()
; - (cons String List-of-Strings)

; String List-of-Strings -> Boolean
; determine whether target is occurs on alos
(define (contains? target alos) (cond [(empty? alos) #false]
                                      [(cons? alos) (cond [(string=? (first alos) target) #true]
                                                          [else (contains? target (rest alos))])]))

(check-expect (contains? "abc" '()) #false)
(check-expect (contains? "abc" (cons "abc" '())) #true)
(check-expect (contains? "abc" (cons "d" '()))#false)
(check-expect (contains? "abc" (cons "d" (cons "abc" '()))) #true)
(check-expect (contains? "abc" (cons "d" (cons "c" '()))) #false)
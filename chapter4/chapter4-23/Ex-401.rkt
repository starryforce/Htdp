#lang htdp/isl+

#| Exercise 401.
Design sexp=?, a function that determines whether two S-expressions are equal.

Whenever you use check-expect,
it uses a function like sexp=? to check whether the two arbitrary values are equal.
If not, the check fails and check-expect reports it as such. 
|#

; For convenience, here is the data definition in condensed form:
; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol
(define ex1 9)
(define ex2 "hello")
(define ex3 's)
(define ex4 '(9 "hello" s))
(define ex5 (list ex1 ex2 ex3 ex4))

; S-expr S-expr -> Boolean
; determines whether s1 & s2 are equal
(define (sexp=? s1 s2)
  (cond [(and (atom? s1) (atom? s2)) (equal? s1 s2)]
        [(and (atom? s1) (or (cons? s2) (empty? s2))) #f]
        [(and (or (cons? s1) (empty? s1)) (atom? s2)) #f]
        [(and (empty? s1) (empty? s2)) #t]
        [(and (empty? s1) (cons? s2)) #f]
        [(and (cons? s1) (empty? s2)) #f]
        [(and (cons? s1) (cons? s1)) (and (sexp=? (first s1) (first s2))
                                          (sexp=? (rest s1) (rest s2)))]))


(check-expect (sexp=? 's 6) #f)
(check-expect (sexp=? "hello" "hello") #t)
(check-expect (sexp=? 9 ex4) #f)
(check-expect (sexp=? ex4 'q) #f)
(check-expect (sexp=? ex4 ex5) #f)
(check-expect (sexp=? ex5 ex5) #t)
(check-expect (sexp=? '() '()) #t)


; Any -> Boolean
; determine if a value is an Atom
; An Atom is one of: 
; – Number
; – String
; – Symbol 
(define (atom? s)
  (or (string? s)
      (number? s)
      (symbol? s)))

(check-expect (atom? "1") #t)
(check-expect (atom? 1) #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? (list 1)) #false)
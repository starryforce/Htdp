#lang htdp/isl+

(define WRONG "not a valid BSL-expr")

; An S-expr is one of: 
; – Atom
; – SL

; An SL is one of: 
; – '()
; – (cons S-expr SL)
          
; An Atom is one of: 
; – Number
; – String
; – Symbol 

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

; A BSL-expr is one of the following:
; - Number
; - Add
; - Mul

(define-struct add [left right])
; An Add is a structure:
; (make-add BSL-expr BSL-expr)
; interpreation (make-add a b) represent sum of a & b
(define ex1 (make-add 1 1))

(define-struct mul [left right])
; A Mul is a structure:
; (make-mul BSL-expr BSL-expr)
; interpretation (make-mul a b) represent muliply of a & b
(define ex2 (make-mul 300001 100000))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

(check-expect (parse 3) 3)
(check-error (parse "1") WRONG)
(check-error (parse 'a) WRONG)
(check-error (parse "abc") WRONG)
(check-expect (parse '(* (+ 3 2) 2)) (make-mul (make-add 3 2) 2))
(check-error (parse '()) WRONG)
(check-error (parse '(+ 1 1 1)) WRONG)
(check-expect (parse '(* 2 4)) (make-mul 2 4))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))

(check-expect (parse-sl '(+ 1 1)) (make-add 1 1))
(check-expect (parse-sl '(* (+ 3 2) 2)) (make-mul (make-add 3 2) 2))
(check-error (parse-sl '("abc" 3)) WRONG)
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(check-expect (parse-atom 3) 3)
(check-error (parse-atom "1") WRONG)
(check-error (parse-atom 'a) WRONG)
 
; SL -> Boolean
; determine if s is a list with 3 and only 3 items
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

(check-expect (consists-of-3 '()) #f)
(check-expect (consists-of-3 '(+ 1 2)) #t)
(check-expect (consists-of-3 '(+ 1 2 3)) #f)
(check-expect (consists-of-3 123) #f)
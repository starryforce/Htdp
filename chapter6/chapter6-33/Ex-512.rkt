#lang htdp/isl+

#| Exercise 512.
Define is-var?, is-λ?, and is-app?, that is,
predicates that distinguish variables from λ expressions and applications.
|#

; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

; 1.
(define ex1 'x)

; 2.
(define ex2 '(λ (x) x))
(define ex3 '(λ (x y) x))
(define ex4 '(λ (x y) (λ (x) x)))

; 3.
(define ex5 '(x y))
(define ex6 '(x (λ (x) x)))
(define ex7 '((λ (x) x) y))
(define ex8 '((λ (x) x) (λ (x) x)))
(define ex9 '((λ (x) x) (x y)))
(define ex10 '((x y) (λ (x) x)))
(define ex11 '((x y) (x y)))
; 2.
(define ex12 '(λ (x y) (x y)))
(define ex13 '(λ (x y) ((λ (x) x) y)))

; Lam -> Boolean
; determine if x is a symbol
(check-expect (is-var? ex1) #t)
(check-expect (is-var? ex2) #f)
(define (is-var? x) (symbol? x))


; Lam -> Boolean
; determine if x is a λ expression
(check-expect (is-λ? ex1) #f)
(check-expect (is-λ? ex2) #t)
(check-expect (is-λ? ex5) #f)
(check-expect (is-λ? ex12) #t)
(define (is-λ? x)
  (and (cons? x) (symbol? (first x)) (symbol=? (first x) 'λ)))

; Lam -> Boolean
; determine if x is an function application
(check-expect (is-app? ex1) #f)
(check-expect (is-app? ex2) #f)
(check-expect (is-app? ex4) #f)
(check-expect (is-app? ex5) #t)
(check-expect (is-app? ex10) #t)
(check-expect (is-app? ex12) #f)
(define (is-app? x)
  (and (not (is-var? x)) (not (is-λ? x))))

; Lam -> (list Symbol)
; extracts the parameter from a λ expression;
(check-expect (λ-para ex2) '(x))
(check-expect (λ-para ex3) '(x y))
(check-expect (λ-para ex13) '(x y))
(define (λ-para x) (second x))

; Lam -> Lam
; extracts the body from a λ expression;
(check-expect (λ-body ex2) 'x)
(check-expect (λ-body ex3) 'x)
(check-expect (λ-body ex12) '(x y))
(check-expect (λ-body ex13) '((λ (x) x) y))
(define (λ-body x) (third x))

; Lam -> Lam 
; extracts the function from an application
(check-expect (app-fun ex5) 'x)
(check-expect (app-fun ex6) 'x)
(check-expect (app-fun ex11) '(x y))
(define (app-fun x) (first x))

; Lam -> Lam 
; extracts the argument from an application.
(check-expect (app-arg ex5) 'y)
(check-expect (app-arg ex7) 'y)
(check-expect (app-arg ex11) '(x y))
(define (app-arg x) (second x))
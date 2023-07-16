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
(define ex4 '(λ (x) (λ (x) x)))

; 3.
(define ex5 '(x y))
(define ex6 '(x (λ (x) x)))
(define ex7 '((λ (x) x) y))
(define ex8 '((λ (x) x) (λ (x) x)))
(define ex9 '((λ (x) x) (x y)))
(define ex10 '((x y) (λ (x) x)))
(define ex11 '((x y) (x y)))
; 2.
(define ex12 '(λ (x) (x y)))
(define ex13 '(λ (x) ((λ (x) x) y)))

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
(check-expect (λ-para ex2) 'x)
(check-expect (λ-para ex13) 'x)
(define (λ-para x) (first (second x)))

; Lam -> Lam
; extracts the body from a λ expression;
(check-expect (λ-body ex2) 'x)
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

; Lam -> [List-of Symbol]
; produces the list of all symbols used as λ parameters
(check-expect (declareds/a ex1) '())
(check-expect (declareds/a ex4) '(x x))
(check-expect (declareds/a ex8) '(x x))
(check-expect (declareds/a ex10) '(x))
(check-expect (declareds/a ex13) '(x x))
(define (declareds/a x0)
  (local (; Lam [List-of Symbol] -> [List-of Symbol]
          ; accumulator a is all λ parameters in that x lacks from x0
          (define (declareds/a x a)
            (cond [(is-var? x) a]
                  [(is-λ? x) (declareds/a (λ-body x) (append a (list (λ-para x))))]
                  [(is-app? x) (append a (declareds/a (app-fun x) '()) (declareds/a (app-arg x) '()))])))
    (declareds/a x0 '())))

(define (declareds-structual x)
  (cond [(is-var? x) '()]
        [(is-λ? x) (cons (λ-para x) (declareds-structual (λ-body x))) ]
        [(is-app? x) (append (declareds-structual (app-fun x)) (declareds-structual (app-arg x)))]))



#lang htdp/isl+

#| Exercise 514.
Make up an ISL+ expression in which x occurs both free and bound.
Formulate it as an element of Lam.
Does undeclareds work properly on your expression?
|#

; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; Lam -> Boolean
; determine if x is a symbol
(define (is-var? x) (symbol? x))

; Lam -> Boolean
; determine if x is a λ expression
(define (is-λ? x)
  (and (cons? x) (symbol? (first x)) (symbol=? (first x) 'λ)))

; Lam -> Boolean
; determine if x is an function application
(define (is-app? x)
  (and (not (is-var? x)) (not (is-λ? x))))

; Lam -> (list Symbol)
; extracts the parameter from a λ expression;
(define (λ-para x) (first (second x)))

; Lam -> Lam
; extracts the body from a λ expression;
(define (λ-body x) (third x))

; Lam -> Lam 
; extracts the function from an application
(define (app-fun x) (first x))

; Lam -> Lam 
; extracts the argument from an application.
(define (app-arg x) (second x))

; Lam -> Lam
(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; works properly

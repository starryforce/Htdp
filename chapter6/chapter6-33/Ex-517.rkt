#lang htdp/isl+

; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

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

(define ex '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
  
; Lam -> Lam
(check-expect (static-distance ex) '((λ () ((λ () (0 1)) 0)) (λ () 0)))
(define (static-distance le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) (nth le declareds) '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ '()
                   (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
               (list (undeclareds/a fun declareds)
                     (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; Any [List-of Any] -> Number
; determine t is ith item in l
(check-expect (nth 1 '(1 2 3)) 0)
(check-expect (nth 2 '(1 2 3)) 1)
(define (nth t l0)
  (local ((define (nth/a l a)
            (cond [(equal? t (first l)) a]
                  [else (nth/a (rest l) (add1 a))])))
    (nth/a l0 0)))
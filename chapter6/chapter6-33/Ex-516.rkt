#lang htdp/isl+

; Exercise 516.
; Redesign the undeclareds function for the structure-based data representation from exercise 513.

(define-struct la [para body])
; A La is a structure:
; (make-la Symbol Lam)
; representation (make-la s l) , s is parameter, l is body
(define-struct app [fun arg])
; An App is a structure:
; (make-app Lam Lam)
; representation (make-app f a),
; f is the function from an application,
; a is the argument from an application

; A Lam is one of: 
; – a Symbol
; – La
; – App

(define ex1 (make-la 'x 'x))
(define ex2 (make-la 'x 'y))
(define ex3 (make-la 'y (make-la 'x 'y)))
(define ex4 (make-app (make-la 'x (make-app 'x 'x)) (make-la 'x (make-app 'x 'x))))

; Lam -> Lam
(check-expect (undeclareds ex1) (make-la 'x 'declared:x))
(check-expect (undeclareds ex2) (make-la 'x '*undeclared:y))
(check-expect (undeclareds ex3) (make-la 'y (make-la 'x 'declared:y)))
(check-expect (undeclareds ex4) (make-app (make-la 'x (make-app 'declared:x 'declared:x))
                                          (make-la 'x (make-app 'declared:x 'declared:x))))
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(symbol? le) (string->symbol
                             (string-append (if (member? le declareds)
                                                "declared:"
                                                "*undeclared:")
                                            (symbol->string le)))]
              [(la? le)
               (local ((define para (la-para le))
                       (define body (la-body le))
                       (define newd (cons para declareds)))
                 (make-la para
                          (undeclareds/a body newd)))]
              [(app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (make-app (undeclareds/a fun declareds)
                           (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))
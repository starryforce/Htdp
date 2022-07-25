#lang htdp/bsl


(define-struct spider [legs space])
; A Spider is a structure:
; (make-spider Number Number)
; interpretation: (make-spider l s) represent a spider
; which have l legs left and the space that it need to
; transport is s

(define-struct elephant [space])
; An Elephant is a structure:
; (make-elephant Number)
; interpretation: (make-elephant s) represent an elephant
; need s space for transport

(define-struct boa-constrictor [length girth])
; A BoaConstrictor is a structure:
; (make-boa-constrictor Number Number)
; interpreation: (make-boa-constrictor l g) represent a boa constrictor
; which length is l, and girth is g

(define-struct armadillo [age space])
; A Armadillo is a structure:
; (make-armadillo Number Number)
; interpretation: (make-armadillo a s) represent a armadillo
; which age is a, and need s space for transport


; ZooAnimal is one of the following:
; - Spider
; - Elephant
; - BoaConstrictor
; - Armadillo
(define ex1 (make-spider 4 5))
(define ex2 (make-elephant 10))
(define ex3 (make-boa-constrictor 5 1))
(define ex4 (make-armadillo 3 6))

; ZooAnimal -> unknown
; todo sth.
(define (f animal) (cond [(spider? animal) ...]
                         [(elephant? animal) ...]
                         [(boa-constrictor? animal) ...]
                         [(armadillo? animal) ...]))


; BoaConstrictor -> Number;
; calculate the space of b
; girth = 2 * PI * r
; space = PI * r^2 * length
(define (query-space b) (* (sqr (/ (boa-constrictor-girth b) 2 pi))
                           2
                           (boa-constrictor-length b)))

(check-within (query-space ex3)
              (* (sqr (/ (boa-constrictor-girth ex3) 2 pi)) 2 (boa-constrictor-length ex3))
              0.0001)

; Cage is a Positive Number;

; ZooAnimal Cage -> Boolean
; determine is cage large enough for ainmal
(define (fits? animal cage) (cond [(spider? animal) (>= cage (spider-space animal))]
                         [(elephant? animal) (>= cage (elephant-space animal) )]
                         [(boa-constrictor? animal) (>= cage (query-space animal))]
                         [(armadillo? animal) (>= cage (armadillo-space animal))]))

(check-expect (fits? ex1 4) #false)
(check-expect (fits? ex1 5) #true)
(check-expect (fits? ex2 5) #false)
(check-expect (fits? ex2 10) #true)
(check-expect (fits? ex3 5) #true)
(check-expect (fits? ex3 5) #true)
(check-expect (fits? ex4 5) #false)
(check-expect (fits? ex4 6) #true)
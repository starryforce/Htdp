#lang htdp/isl+

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; A Child is a structure: 
;   (make-child Child Child String N String)


(define NP (make-no-parent))
; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)


; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))



; FT -> Number
; count how many child does an-ftree have
(define (count-persons an-ftree)
  (cond [(no-parent? an-ftree) 0]
        [else (+ 1
                 (count-persons (child-father an-ftree))
                 (count-persons (child-mother an-ftree)))]))

(check-expect (count-persons Dave) 3)
(check-expect (count-persons Gustav) 5)
(check-expect (count-persons Carl) 1)
#lang htdp/isl+

(define-struct no-parent [])
(define-struct children [father mother name date eyes])
; A Child is a structure: 
;   (make-child Child Child String N String)


(define NP (make-no-parent))
; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)


; Oldest Generation:
(define Carl (make-children NP NP "Carl" 1926 "green"))
(define Bettina (make-children NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-children Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-children Carl Bettina "Dave" 1955 "black"))
(define Eva (make-children Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-children NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-children Fred Eva "Gustav" 1988 "brown"))


; FT -> [List-of String]
; produces a list of all eye colors in an-ftree
(define (eye-colors an-ftree)
  (cond
    [(no-parent? an-ftree) '()]
    [else (cons (children-eyes an-ftree)
                (append (eye-colors (children-father an-ftree))
                        (eye-colors (children-mother an-ftree))
                        ))]))

(check-expect (length (eye-colors Dave)) 3)
(check-expect (length (eye-colors Gustav)) 5)
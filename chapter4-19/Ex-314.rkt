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

; An FF (short for family forest) is:
; [List-of FT]
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees


(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))


; [List-of FT] -> Boolean
; does the forest contain any child with "blue" eyes


(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)
 
 
(define (blue-eyed-child-in-forest? a-forest)
  (ormap (lambda (t) (blue-eyed-child? t))
         a-forest))

(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))
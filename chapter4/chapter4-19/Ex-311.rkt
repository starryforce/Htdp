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


; FT Number -> Number
; produces the average age of all child structures in an-tree,
; at year cy
(define (average-age an-ftree cy)
  (/ (total-age an-ftree cy)
     (count-persons an-ftree)))

; FT -> Number
; count how many child does an-ftree have
(define (count-persons an-ftree)
  (cond [(no-parent? an-ftree) 0]
        [else (+ 1
                 (count-persons (child-father an-ftree))
                 (count-persons (child-mother an-ftree)))]))

; FT Number -> Number
; calc total age of all person in an-ftree
(define (total-age an-ftree cy)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (total-age (child-father an-ftree) cy)
             (total-age (child-mother an-ftree) cy)
               (- cy (child-date an-ftree)))]))

(check-expect (total-age Carl 1936) 10)
(check-expect (total-age Adam 1960) (+ 34 34 10 ))
(check-expect (total-age Gustav 1990) (+ (- 1990 1926)
                                         (- 1990 1926)
                                         (- 1990 1965)
                                         (- 1990 1966)
                                         (- 1990 1988)))


(check-expect (average-age Carl 1936) 10)
(check-expect (average-age Adam 1960) (/ (+ 34 34 10 ) 3))
(check-expect (average-age Gustav 1990) (/ (+ (- 1990 1926)
                                              (- 1990 1926)
                                              (- 1990 1965)
                                              (- 1990 1966)
                                              (- 1990 1988)) 5))

#lang htdp/isl+

#|
Exercise 315. Design the function average-age.
It consumes a family forest and a year (N).
From this data, it produces the average age of all child instances in the forest.
Note If the trees in this forest overlap,
 the result isn’t a true average because some people contribute more than others.
For this exercise, act as if the trees don’t overlap. 
|#

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


(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; FT Number -> Number
; calc total age of all person in an-ftree
(define (total-age an-ftree cy)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (total-age (child-father an-ftree) cy)
             (total-age (child-mother an-ftree) cy)
             (- cy (child-date an-ftree)))]))

; FT -> Number
; count how many child does an-ftree have
(define (count-persons an-ftree)
  (cond [(no-parent? an-ftree) 0]
        [else (+ 1
                 (count-persons (child-father an-ftree))
                 (count-persons (child-mother an-ftree)))]))

; [List-of FT] Number -> Number
; produces the average age of all child instances in a-forest
(define (average-age a-forest N)
  (/ (foldl (lambda (cur prev) (+ (total-age cur N) prev)) 0 a-forest)
     (foldl (lambda (cur prev) (+ (count-persons cur) prev)) 0 a-forest)))


(check-expect (average-age ff1 1930) 4)
(check-expect (average-age ff2 1970) (/ (+ (- 1970 1966)
                                           (- 1970 1965)
                                           (- 1970 1926)
                                           (- 1970 1926)) 4))
(check-expect (average-age ff3 1980) (/ (+ (- 1980 1966)
                                           (- 1980 1965)
                                           (- 1980 1926)
                                           (- 1980 1926)
                                           (- 1980 1926)) 5))
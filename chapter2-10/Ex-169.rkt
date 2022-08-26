#lang htdp/bsl

; Lop (short for list of posns) is one of:
; - '()
; - (cons Posn LOP)
; interpretation an instance of Lop
; represents a list of Posns

(define ex0 '())
(define ex1 (cons (make-posn 10 20) '()))
(define ex2 (cons (make-posn 102 50) '()))
(define ex3 (cons (make-posn 15 50) ex2))

; Lop -> Lop
; for every posn in alop, add their posn-y by 1
(define (legal alop)
  (cond [(empty? alop) '()]
        [else (if (check (first alop))
                  (cons (first alop) (legal (rest alop)))
                  (legal (rest alop)))]))

(check-expect (legal ex0) '())
(check-expect (legal ex1) ex1)
(check-expect (legal ex2) '())
(check-expect (legal ex3) (cons (make-posn 15 50) '()))

(define ex11 (make-posn -50 100))
(define ex12 (make-posn 50 100))

; Posn -> Boolean
; determine if (posn-x p) is between between 0 and 100
; and (posn-y p) is between between 0 and 200
(define (check p) (and (>= (posn-x p) 0)
                       (<= (posn-x p) 100)
                       (>= (posn-y p) 0)
                       (<= (posn-y p) 200)))

(check-expect (check ex11) #false)
(check-expect (check ex12) #true)


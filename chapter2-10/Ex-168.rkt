#lang htdp/bsl

; Lop (short for list of posns) is one of:
; - '()
; - (cons Posn LOP)
; interpretation an instance of Lop
; represents a list of Posns

(define ex0 '())
(define ex1 (cons (make-posn 10 20) '()))
(define ex2 (cons (make-posn 15 50) ex1))

; Lop -> Lop
; for every posn in alop, add their posn-y by 1
(define (translate alop)
  (cond [(empty? alop) '()]
        [else (cons (addy1 (first alop))
                    (translate (rest alop)))]))

(check-expect (translate ex0) '())
(check-expect (translate ex1) (cons (make-posn 10 21) '()))
(check-expect (translate ex2) (cons (make-posn 15 51) (cons (make-posn 10 21) '())))


; Posn -> Posn
; change (posn-y p) to (+ (posn-y p) 1)
(define (addy1 p) (make-posn (posn-x p) (+ 1 (posn-y p))))

(check-expect (addy1 (make-posn 10 21)) (make-posn 10 22))


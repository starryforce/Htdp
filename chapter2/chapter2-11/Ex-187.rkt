#lang htdp/bsl+

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

(define ex1 (make-gp "a" -1))
(define ex2 (make-gp "b" 0))
(define ex3 (make-gp "c" 1))

; A List-of-gameplayers is one of;
; - '()
; - (cons GamePlayer  List-of-gameplayers)

; List-of-gameplayers -> List-of-gameplayers
; rearranges alog in descending order
(define (sort> alog)
  (cond [(empty? alog) '()]
        [else (insert (first alog)
                      (sort> (rest alog)))]))

(check-expect (sort> '()) '())
(check-expect (sort> (list ex1)) (list ex1))
(check-expect (sort> (list ex1 ex2 ex3)) (list ex3 ex2 ex1))
(check-expect (sort> (list ex3 ex2 ex1)) (list ex3 ex2 ex1))
(check-expect (sort> (list ex1 ex3 ex2)) (list ex3 ex2 ex1))

; GamePlayer List-of-gameplayers -> List-of-gameplayers
; inserts g into the sorted list of gameplayer alog
(define (insert g alog)
  (cond
    [(empty? alog) (list g)]
    [else (if (higher? g (first alog))
              (cons g alog)
              (cons (first alog) (insert g (rest alog))))]))

(check-expect (insert ex1 '()) (list ex1))
(check-expect (insert ex2 (list ex1)) (list ex2 ex1))
(check-expect (insert ex2 (list ex3)) (list ex3 ex2))
(check-expect (insert ex2 (list ex3 ex1)) (list ex3 ex2 ex1))

; GamePlayer GamePlayer -> Boolean
; determine if g1's score is higher than or equal g'2 score
(define (higher? g1 g2)
  (>= (gp-score g1) (gp-score g2)))

(check-expect (higher? ex1 ex2) #false)
(check-expect (higher? ex2 ex1) #true)
(check-expect (higher? ex1 ex1) #true)

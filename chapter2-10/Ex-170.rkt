#lang htdp/bsl

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.

(define ex1 (make-phone 911 123 1234))
(define ex2 (make-phone 713 789 8765))
(define ex3 (make-phone 289 713 5678))

; Lop (short for list of phone) is one of:
; - '()
; - (cons Phone Lop)
; interpretation an instance of Lop represent
; a list of phone numbers

(define ex10 '())
(define ex11 (cons ex1 '()))
(define ex12 (cons ex2 ex11))
(define ex13 (cons ex3 ex12))

; Lop -> Lop
; replace all phone i alop which area code is 713 with 281
(define (replace alop)
  (cond [(empty? alop) '()]
        [else (cons (update (first alop))
                    (replace (rest alop)))]))

(check-expect (replace ex10) '())
(check-expect (replace ex11) ex11)
(check-expect (replace ex12) (cons (make-phone 281 789 8765) ex11))
(check-expect (replace ex13) (cons ex3 (cons (make-phone 281 789 8765) ex11)))

; Phone -> Phone
; if p's (phone-area p) is 713,replace it with 281
(define (update p) (make-phone (if (= (phone-area p) 713) 281 (phone-area p))
                             (phone-switch p)
                             (phone-four p)))

(check-expect (update ex1) ex1)
(check-expect (update ex2) (make-phone 281 789 8765))
(check-expect (update ex3) ex3)

#lang htdp/bsl

(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

(define ex0 '())
(define ex1 (cons (make-work "Robby" 11.95 39)
                  '()))
(define ex2 (cons (make-work "Matthew" 12.95 45)
                  (cons (make-work "Robby" 11.95 39)
                        '())))
(define ex3 (cons (make-work "Jacky" 28.34 12)
                  (cons (make-work "Tom" 59.23 40)
                        '())))

; Low -> List-of-numbers
; computes the weekly wages for all weekly work records 
 
(check-expect
  (wage*.v2 (cons (make-work "Robby" 11.95 39) '()))
  (cons (* 11.95 39) '()))

; Low -> List-of-numbers
; computes the weekly wages for the given records
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (wage.v2 (first an-low))
                          (wage*.v2 (rest an-low)))]))
 
; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))


(check-expect (wage*.v2 ex0) '())
(check-expect (wage*.v2 ex1) (cons 466.05 '()))
(check-expect (wage*.v2 ex2) (cons 582.75 (cons 466.05 '())))

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

; Low -> List-of-numbers
; computes the weekly wages for all weekly work records 
 
(check-expect
  (wage*.v2 (cons (make-work "Robby" 11.95 39) '()))
  (cons (* 11.95 39) '()))
 
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (wage.v2 (first an-low))
                          (wage*.v2 (rest an-low)))]))
 
; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

(define-struct paycheck [name amount])
; A Paycheck is a structure:
; (make-paycheck String Number)
; interpretation (make n a) combines the name n
; with the amount a which represent the amount of wages

; Lop (short for List of paychecks) is one of:
; - '()
; (cons Paycheck Lop)
; interpretation an intance of Lop represets  a number of paychecks

; Low -> Lop
; computes the paycheck info for an-low
(define (wage*.v3 an-low)
  (cond [(empty? an-low) '()]
        [else (cons (handle-work (first an-low))
                    (wage*.v3 (rest an-low)))]))

(check-expect
  (wage*.v3 (cons (make-work "Robby" 11.95 39) '()))
  (cons (make-paycheck "Robby" (* 11.95 39)) '()))

; Work -> Paycheck
; calc paycheck from Work w
(define (handle-work w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))

(check-expect (handle-work (make-work "Robby" 11.95 39))
              (make-paycheck "Robby" (* 11.95 39)))
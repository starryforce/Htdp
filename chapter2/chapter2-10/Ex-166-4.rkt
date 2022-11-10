#lang htdp/bsl

(define-struct employee [no name])
; An Employee is a structure:
; (make-employee String String)
; interpretation (make-employee no n) represent
; an employee names n which employee number is no,

(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work Employee Number Number)
; interpretation (make-work n r h) combines the employee info
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

(define-struct paycheck [employee amount])
; A Paycheck is a structure:
; (make-paycheck Employee Number)
; interpretation (make e a) combines the employee info e
; with the amount a which represent the amount of wages

; Lop (short for list of paychecks) is one of:
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
  (wage*.v3 (cons (make-work (make-employee "s1949" "Robby") 11.95 39) '()))
  (cons (make-paycheck (make-employee "s1949" "Robby") (* 11.95 39)) '()))

; Work -> Paycheck
; calc paycheck from Work w
(define (handle-work w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))

(check-expect (handle-work (make-work (make-employee "s1949" "Robby") 11.95 39))
              (make-paycheck (make-employee "s1949" "Robby") (* 11.95 39)))
#lang htdp/isl+

#| Exercise 388. 
In the real world,
wages*.v2 consumes lists of employee structures and lists of work records.
An employee structure contains an employee’s name, social security number, and pay rate.
A work record also contains an employee’s name and the number of hours worked in a week.
The result is a list of structures that contain the name of the employee and the weekly wage.

Modify the program in this section so that it works on these realistic versions of data.
Provide the necessary structure type definitions and data definitions.
Use the design recipe to guide the modification process. 
|#

(define-struct employee [name id rate])
; An Employee is a structure:
; (make-employee String String Number)
; interpreation (make-employee n i r)
; represents n's social security number is i, his pay rate is r
(define ex-e1 (make-employee "Tom" "001" 5.65))
(define ex-e2 (make-employee "Jerry" "002" 8.75))

(define-struct record [name hours])
; A Record is a structure:
; (make-record String Number)
; interpretation (make-record n h) represents a record
; contains an employee’s name and the number of hours worked in a week
(define ex-r1 (make-record "Tom" 40.0))
(define ex-r2 (make-record "Jerry" 30.0))

(define-struct fee [name wage])
; An Fee is a structure:
; (make-fee String Number)
; interpretation (make-fee n w) represents n's weekly wage is w.
(define ex-f1 (make-fee "Tom" 226.0))
(define ex-f2 (make-fee "Jerry" 262.5))

; [List-of Employee] [List-of Record] -> [List-of Fee]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length 
(define (wages*.v2 employees records)
  (cond
    [(empty? employees) '()]
    [else
     (cons
      (make-fee (employee-name (first employees))
                (weekly-wage (employee-rate (first employees))
                             (record-hours (first records))))
                (wages*.v2 (rest employees) (rest records)))]))


(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list ex-e1) (list ex-r1))
              (list ex-f1))
(check-expect (wages*.v2 (list ex-e1 ex-e2) (list ex-r1 ex-r2))
              (list ex-f1 ex-f2))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))
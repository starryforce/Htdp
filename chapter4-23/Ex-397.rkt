#lang htdp/isl+

#| Exercise 397.
In a factory, employees punch time cards as they arrive in the morning and leave in the evening.
Electronic time cards contain an employee number and record the number of hours worked per week.
Employee records always contain the name of the employee, an employee number, and a pay rate.

Design wages*.v3. The function consumes a list of employee records and a list of time-card records.
It produces a list of wage records, which contain the name and weekly wage of an employee.
The function signals an error if it cannot find an employee record for a time card or vice versa.

Assumption There is at most one time card per employee number. 
|#

(define-struct card [no hours])
; A Card is a structure:
; (make-card String Number)
; interpretation (make n h) represent an electronic time card which
; no is n and works h hours per week
(define ex-c1 (make-card "01" 20))
(define ex-c2 (make-card "02" 25))

(define-struct employee [name no rate])
; An Employee is a structure:
; (make-employee String String Number)
; interpretation (make-employee na no r) represents an employee
; whose name is n and employee number is no with pay rate r
(define ex-e1 (make-employee "Tom" "01" 3))
(define ex-e2 (make-employee "Jerry" "02" 2))

(define-struct wage [name amount])
; A Wage is a structure:
; (make-wage String Number)
; interpretation (make-wage n a) represents a wage record for n,
; whose weekly wage is a
(define ex-w1 (make-wage "Tom" 60))
(define ex-w2 (make-wage "Jerry" 50))

(define ERR-CARD "can't found relate card record")
(define ERR-EMPLOYEE "can't found relate employee record")

; [List-of Employee] [List-of Card] -> [List-of Wage]
; produces a list of wage records, which contain the name and weekly wage of an employee.
(define (wages*.v3 aloer alocr)
  (cond [(and (empty? aloer) (empty? alocr)) '()]
        [(and (empty? aloer) (cons? alocr)) (error ERR-EMPLOYEE)]
        [(and (cons? aloer) (empty? alocr)) (error ERR-CARD)]
        [(and (cons? aloer) (cons? alocr)) (cons (calc-wage (first aloer) (query-relate (first aloer) alocr))
                                                 (wages*.v3 (rest aloer) (remove-relate (first aloer) alocr)))]))

(check-expect (wages*.v3 '() '()) '())
(check-error (wages*.v3 (list ex-e1) '()) ERR-CARD)
(check-error (wages*.v3 (list ex-e1 ex-e2) (list ex-c1)) ERR-CARD)
(check-error (wages*.v3 '() (list ex-c1)) ERR-EMPLOYEE)
(check-error (wages*.v3 (list ex-e1) (list ex-c1 ex-c2)) ERR-EMPLOYEE)
(check-expect (wages*.v3 (list ex-e1) (list ex-c1)) (list ex-w1))
(check-expect (wages*.v3 (list ex-e1 ex-e2) (list ex-c1 ex-c2)) (list ex-w1 ex-w2))

; Employee [List-of Card] -> Card
; find the card related to e in alocr
(define (query-relate e alocr)
  (cond [(empty? alocr) (error ERR-CARD)]
        [else (if (string=? (employee-no e) (card-no (first alocr)))
                  (first alocr)
                  (query-relate e (rest alocr)))]))

(check-expect (query-relate ex-e1 (list ex-c1)) ex-c1)
(check-error (query-relate ex-e1 (list ex-c2)) ERR-CARD)

; Employee [List-of Card] -> [List-of Card]
; remove the card reltaed to e in alocr
(define (remove-relate e alocr)
  (cond [(empty? alocr) '()]
        [else (if (string=? (employee-no e) (card-no (first alocr)))
                  (remove-relate e (rest alocr))
                  alocr)]))

(check-expect (remove-relate ex-e1 (list ex-c1 ex-c2)) (list ex-c2))

; Employee Card -> Wage
; calculate the wage of e according to record c
(define (calc-wage e c)
  (make-wage (employee-name e)
             (* (employee-rate e) (card-hours c))))

(check-expect (calc-wage ex-e1 ex-c1) ex-w1)
#lang htdp/bsl+

(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time

(define ex1 (make-email "a" 1 "hello"))
(define ex2 (make-email "b" 10 "world"))
(define ex3 (make-email "c" 100 "bye"))

; A List-of-emails is one of:
; - '()
; (cons Email List-of-emails)


; List-of-emails -> List-of-emails
; rearrange aloe by in descending order by name date use the string<? 
(define (sort> aloe)
  (cond [(empty? aloe) '()]
        [else (insert (first aloe) (sort> (rest aloe)))]))

(check-expect (sort> '()) '())
(check-expect (sort> (list ex1)) (list ex1))
(check-expect (sort> (list ex1 ex2 ex3)) (list ex1 ex2 ex3))
(check-expect (sort> (list ex3 ex2 ex1)) (list ex1 ex2 ex3))
(check-expect (sort> (list ex1 ex3 ex2)) (list ex1 ex2 ex3))


; Email List-of-emails -> List-of-emails
; insert Email e into descending order List of emails aloe
(define (insert e aloe)
  (cond [(empty? aloe) (list e)]
        [else (if (before? e (first aloe))
                  (cons e aloe)
                  (cons (first aloe) (insert e (rest aloe))))]))

(check-expect (insert ex1 '()) (list ex1))
(check-expect (insert ex1 (list ex2 ex3)) (list ex1 ex2 ex3))
(check-expect (insert ex3 (list ex1 ex2)) (list ex1 ex2 ex3))
(check-expect (insert ex2 (list ex1 ex3)) (list ex1 ex2 ex3))


; Email Email -> Boolean
; determines if e1's name is before to e2's name
(define (before? e1 e2)
  (string<? (email-from e1) (email-from e2)))

(check-expect (before? ex1 ex2) #true)
(check-expect (before? ex3 ex2) #false)
(check-expect (before? ex1 ex1) #false)


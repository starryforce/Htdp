#lang htdp/isl+

#| Exercise 381.
The definitions of XMachine and X1T use quote,
which is highly inappropriate for novice program designers.
Rewrite them first to use list and then cons.
|#

(define FSM-State "red")


; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))
(define ex1 `(action ((state ,FSM-State) (next ,FSM-State))))
; list
;   (list 'action (list (list 'state FSM-State) (list 'next FSM-State)))
(define ex1-list (list 'action (list (list 'state FSM-State) (list 'next FSM-State))))
(check-expect ex1 ex1-list)
; cons
;   (cons 'action (cons (cons (cons 'state (cons FSM-State '())) (cons (cons 'next (cons FSM-State '())) '())) '()))
(define ex1-cons (cons 'action (cons (cons (cons 'state (cons FSM-State '())) (cons (cons 'next (cons FSM-State '())) '())) '())))
(check-expect ex1 ex1-cons)

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State))  [List-of X1T]))
(define ex2 (cons 'machine (cons `((initial ,FSM-State)) (list ex1 ex1))))
; list
;   (list 'machine (list (list 'initial FSM-State)) X1T ...) it seems here should use cons?
(define ex2-list (list 'machine (list (list 'initial FSM-State)) ex1 ex1))
(check-expect ex2 ex2-list)
; cons
;   (cons 'machine (cons (cons (cons 'initial (cons FSM-State '())) '()) [List-of X1T]))
(define ex2-cons (cons 'machine (cons (cons (cons 'initial (cons FSM-State '())) '()) (list ex1 ex1))))
(check-expect ex2 ex2-cons)

#lang htdp/bsl

; A Color is one of: 
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"
; e.g. "white" "red"


; H is a Number between 0 and 100.
; interpretation represents a happiness value
; e.g 0 50 100

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)
(make-person "Mike" "Floor" #true)

(define-struct dog [owner name age happiness])
; A Dog is a structure:
;   (make-dog Person String PositiveInteger H)
(make-dog "Mary" "Petty" 12 50)

; A Weapon is one of: 
; — #false
; — Posn
; interpretation #false means the missile hasn't 
; been fired yet; a Posn means it is in flight

#false
(make-posn 100 200)
#lang htdp/isl+

; Any -> Boolean
; determine if a value is an Atom
; An Atom is one of: 
; – Number
; – String
; – Symbol 
(define (atom? s)
  (or (string? s)
      (number? s)
      (symbol? s)))

(check-expect (atom? "1") #t)
(check-expect (atom? 1) #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? (list 1)) #false)
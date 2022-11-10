#lang htdp/isl+

(define RATE 1.06)
; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts 
(define (convert-euro alon)
  (map (lambda (n) (* n RATE)) alon)
  )

(check-expect (convert-euro (list 1 10 100)) (list 1.06 10.6 106))



; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to a list of Celsius measurements
(define (convertFC alon)
  (map (lambda (n) (/ (- n 32) (/ 9 5))) alon))

(check-expect (convertFC (list 68 32 212)) (list 20 0 100))

; A NumberPair is a list with two number:
; (list Number Number)

; [List-of Posn] -> [List-of NumberPair]
; translates a list of Posns into a list of lists of pairs of numbers
(define (translate alop)
  (map (lambda (p)
         (list (posn-x p) (posn-y p))) alop))

(check-expect (translate (list (make-posn 0 1) (make-posn 10 10))) (list (list 0 1) (list 10 10)))
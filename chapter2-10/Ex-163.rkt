#lang htdp/bsl

; Number -> Number 
; converts Fahrenheit temperatures to Celsius
(define (f2c f)
  (* 5/9 (- f 32)))

(check-expect (f2c 32) 0)
(check-expect (f2c 212) 100)
(check-expect (f2c -40) -40)

; List-of-numbers -> List-of-numbers
; converts a list of measurements in Fahrenheit to a list of Celsius measurements
(define (convertFC f)
  (cond [(empty? f) '()]
        [else (cons (f2c (first f)) (convertFC (rest f)))]))

(check-expect (convertFC '()) '())
(check-expect (convertFC (cons 32 '())) (cons 0 '()))
(check-expect (convertFC (cons 32 (cons 212 '()))) (cons 0 (cons 100 '())))
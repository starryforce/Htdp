#lang htdp/isl

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts 
(define (convert-euro alon)
  (local ((define RATE 1.06)
          ; Number -> Number
          ; converts a US$ amounts into a € amounts based on an exchange rate of US$1.06 per € (on April 13, 2017)
          (define (convert n)
            (* n RATE))
          )
    ; - IN -
    (map convert alon)
    ))

(check-expect (convert-euro (list 1 10 100)) (list 1.06 10.6 106))



; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to a list of Celsius measurements
(define (convertFC alon)
  (local (; Number -> Number
          ; converts a Fahrenheit measurements to a Celsius measurements.
          (define (convert n)
            (/ (- n 32) (/ 9 5)))
          )
    (map convert alon)))

(check-expect (convertFC (list 68 32 212)) (list 20 0 100))
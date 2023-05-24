#lang htdp/isl+

(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        #i4.2344294738446e+170
        1.0
        #i-8e+269
        0.0
        99.0))

; [List-of Number] -> Number
; calc the sum of numbers in l
(define (sum l)
  (foldl + 0 l))

(sum JANUS)

(sum (reverse JANUS))

(sum (sort JANUS <))

(exact->inexact (sum (map inexact->exact JANUS)))
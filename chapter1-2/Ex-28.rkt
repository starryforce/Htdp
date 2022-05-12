#lang htdp/bsl

(define (profit price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 180
        (* 0.04
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price)))))))

;(profit 1)
;(profit 2)
;(profit 3)
;(profit 4)
;(profit 5)

;(profit 3)
;(profit 3.5)
;(profit 3.3)
;(profit 3.2)
;(profit 3.1)

;(profit 3)
;(profit 2.5)
(profit 2.8)
(profit 2.9)
(profit 3.0)

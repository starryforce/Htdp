#lang htdp/bsl

; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
; examples: 
;   given: 20, expect 23
;   given: 78, expect 81
(define (tock cw)
  (+ cw 3))

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

;(check-expect (tock 78) 80)
#lang htdp/bsl

(define-struct time-point [hours minutes seconds])
; A TimePoint is a structure:
; (make-time-point Number Number Number)

(define ex1 (make-time-point 15 50 49))
(define ex2 (make-time-point 19 0 0))
(define ex3 (make-time-point 12 30 2))

; TimePoint -> Number
; determines the number of seconds that tp have passed since midnight
(define (time->seconds tp)
  (+ (* (time-point-hours tp) 60 60)
     (* (time-point-minutes tp) 60)
       (time-point-seconds tp)))

(check-expect (time->seconds ex1) (+ (* 15 60 60) (* 50 60) 49))
(check-expect (time->seconds ex2) (* 19 60 60))
(check-expect (time->seconds ex3) 45002)
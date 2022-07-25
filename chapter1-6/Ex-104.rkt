#lang htdp/bsl

; VehicleType is one of the following:
; - automobile
; - van
; - buse
; - SUV

(define automobile "automobile")
(define van "van")
(define buse "buse")
(define SUV "SUV")


(define-struct vehicle [type max no cosume])
; An Vehicle is a structure:
; (make-vehicle VehicleType Number String Number)
; interpretation: (make-vehicle t m n c)
; t: the type of vehicle
; m: the number of passengers that it can carry
; n: its license plate number
; c: its fuel consumption (miles per gallon)

(define (vehicle-consumer v) (cond [(string=? (vehicle-type v) automobile) ...]
                                   [(string=? (vehicle-type v) van) ...]
                                   [(string=? (vehicle-type v) buse) ...]
                                   [(string=? (vehicle-type v) SUV) ...]))
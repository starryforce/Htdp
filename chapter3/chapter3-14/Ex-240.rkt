#lang htdp/isl

(define-struct layer [stuff])

; An LStr is one of: 
; – String
; – (make-layer LStr)
(define ex10 "abc")
(define ex11 (make-layer "abc"))
(define ex12 (make-layer (make-layer "abc")))

; An LNum is one of: 
; – Number
; – (make-layer LNum)
(define ex20 4)
(define ex21 (make-layer 4))
(define ex22 (make-layer (make-layer 4)))


; An [Layer T] is one of:
; - T
; - (make-layer [Layer T])

; [Layer String]

; [Layer Number]
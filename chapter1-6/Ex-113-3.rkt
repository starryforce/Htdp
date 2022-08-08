#lang htdp/bsl

; A Happiness is a number;
; [0,100]
; interpretation: the level of happiness

(define-struct vcat [x-coordinate happiness])
; A VCat is a structure:
; (make-vcat Number Number)
; interpretation (make-vcat x h) describes a virtual
; cat displaying at x position, and it's happiness value is h

(define-struct vcham [position happiness color])
; A VCham is a structure:
; (make-vcham XPosition Happiness ChamColor)
; interpretation: (make-vcham 40 50 "r") represent a chameleon stay at
; x-coordinate of 40, it's happiness level is 50, it's color is red

; A VAnimal is either
; – a VCat
; – a VCham

; Any -> Boolean
; is v an element of the VAnimal
(define (vanimal? v) (cond [(or (vcat? v) (vcham? v)) #true]
                           [else #false]))

(check-expect (vanimal? 1) #false)
(check-expect (vanimal? "hello") #false)
(check-expect (vanimal? #false) #false)
(check-expect (vanimal? (make-vcat 1 2)) #true)
(check-expect (vanimal? (make-vcham 2 3 "red")) #true)
#lang htdp/bsl

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)

; RD -> String
; produces the (color of the) innermost doll of an-rd
(define (inner an-rd)
  (cond [(string? an-rd) an-rd]
        [(layer? an-rd) (inner (layer-doll an-rd))]))

(check-expect (inner "red") "red")
(check-expect
  (inner
   (make-layer "yellow" (make-layer "green" "red")))
  "red")
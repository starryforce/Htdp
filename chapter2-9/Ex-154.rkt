#lang htdp/bsl

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)


; RD -> String;
; produces a string of all colors in an-rd,
; separated by a comma and a space
(define (colors an-rd)
  (cond [(string? an-rd) an-rd]
        [(layer? an-rd)
         (string-append (layer-color an-rd)
                        ", "
                        (colors (layer-doll an-rd)))]
        ))


(check-expect (colors "red") "red")
(check-expect
  (colors
   (make-layer "yellow" (make-layer "green" "red")))
  "yellow, green, red")

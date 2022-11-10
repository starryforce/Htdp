#lang htdp/bsl



(define-struct phone [area number])
; A Phone is a structure:
; (make-phone Number String)
; interpretation a phone have area code and tel number

(define-struct phone# [area switch num])

; A Phone-A is a structure:
; (make-phone# Number Number String)
; interpretation
; area is three digits representing area code,a Number between 000 and 999
; switch is three digits represeting phone switch,a Number between 000 and 999
; num is four digits representing phone with respect to the neighborhood
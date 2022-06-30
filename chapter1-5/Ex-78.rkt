#lang htdp/bsl

; An Element is one of:
; - "a" through "z"
; - #false
; A ThreeLetterWord is (make-three-letter-word Elment Elment Elment)
(define-struct three-letter-word [first second thrid])

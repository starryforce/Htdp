#lang htdp/bsl

; String -> 1String
; extracts the first character from str, which is a non-empty string

;(define (string-first str) "a")

; given:"hello", expect:"h"
; given:"world", expect:"w"


;(define (string-first str
;  (... str ...))


(define (string-first str)
  (substring str 0 1))

(string-first "hello")
(string-first "world")
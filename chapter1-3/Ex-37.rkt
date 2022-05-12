#lang htdp/bsl

; String -> String
; get the string with first character removed

; (define (string-rest str) "abc")

; given: "hello", expect: "ello"
; given: "world", expect: "orld"


;(define (string-rest str)
;  (... str ...))

(define (string-rest str)
  (substring str 1))

(string-rest "hello")
(string-rest "world")


#lang htdp/bsl

; String -> 1String
; extracts the last character from str, which is a non-empty string

; (define (string-last str) "a")

; given: "hello", expect "o"
; given: "world", expect "d"

;(define (string-last str)
;  (... str ...))



(define (string-last str)
  (substring str (- (string-length str) 1)))

(string-last "hello")
(string-last "world")
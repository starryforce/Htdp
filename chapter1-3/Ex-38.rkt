#lang htdp/bsl


; String -> String
; produce a string like the str but remove last character

;(define (string-remove-last str) "xy")

; given: "hello", expect: "hell"
; given: "world", expect: "worl"

;(define (string-remove-last str)
;  (... str ...))

(define (string-remove-last str)
  (if (> (string-length str) 0) (substring str 0 (- (string-length str) 1)) str))


(string-remove-last "hello")
(string-remove-last "world")
(string-remove-last "a")
(string-remove-last "")
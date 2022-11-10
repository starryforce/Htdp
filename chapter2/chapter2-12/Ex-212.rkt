#lang htdp/bsl+

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
(define word0 '())
(define word1 (list "d" "e"))
(define word2 (list "e" "d"))


; A List-of-words is one of:
; - '() or
; - (cons Word List-of-words)
; interpretation a List-of-words is a list of Word (also a list)

(define low1 (list word1 word2))


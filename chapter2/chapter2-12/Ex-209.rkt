#lang htdp/bsl+

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

(define word0 '())
(define word1 (list "e" "g" "g"))
(define word2 (list "d" "o" "g"))


; String -> Word
; converts s to the chosen word representation 
(define (string->word s) (explode s))

(check-expect (string->word "") word0)
(check-expect (string->word "egg") word1)
(check-expect (string->word "dog") word2)
 
; Word -> String
; converts w to a string
(define (word->string w) (implode w))

(check-expect (word->string word0) "")
(check-expect (word->string word1) "egg")
(check-expect (word->string word2) "dog")
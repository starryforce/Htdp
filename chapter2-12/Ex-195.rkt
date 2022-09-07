#lang htdp/bsl+

(require 2htdp/batch-io)

; On OS X: 
; (define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
(define LOCATION "C:/Users/StarryForce/OneDrive/words")
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define ex0 '())
(define ex1 (list "egg" "apple" "big" "small" "eat"))

; Letter Dictionary -> Number
; counts how many words in the given Dictionary dict
; start with the given Letter l
(define (starts-with# l dict)
  (cond [(empty? dict) 0]
        [else (+ (if (starts-with l (first dict)) 1 0)
                 (starts-with# l (rest dict)))]))

(check-expect (starts-with# "a" ex0) 0)
(check-expect (starts-with# "e" ex1) 2)
(check-expect (starts-with# "b" ex1) 1)
(check-expect (starts-with# "z" ex1) 0)

; Letter String -> Boolean
; determine if string s starts width letter l
(define (starts-with l s)
  (if (string=? s "") #false (string=? (string-ith s 0) l)))

(check-expect (starts-with "e" "") #false)
(check-expect (starts-with "e" "egg") #true)
(check-expect (starts-with "a" "egg") #false)

; 3307
(starts-with# "e" AS-LIST)
; 151
(starts-with# "z" AS-LIST)

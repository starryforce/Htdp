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

(define-struct letter-count [letter count])
; A LetterCount is a structure:
; (make-letter-count Letter Number)
; interpretation (make-letter-count l n) combines the letter l
; with the count which it occurs as first ones in sth.

(define letters1 (list "a" "b" "d"))
(define dict1 (list "apple" "bird" "cat" "egg" "fuck" "appear" "banana" "bad"))

; List-of-letters Dictionary -> List-of-letter-counts
; for every letter in alol, count the number of the times it appears at the first letter
; in dict, and generate a record for it,combine all the result in a list 
(define (count-by-letter alol dict)
  (cond [(empty? alol) '()]
        [else (cons (count-letter (first alol) dict)
                    (count-by-letter (rest alol) dict))]))


(check-expect (count-by-letter '() dict1) '())
(check-expect (count-by-letter letters1 dict1) (list (make-letter-count "a" 2)
                                                    (make-letter-count "b" 3)
                                                    (make-letter-count "d" 0)))

; Letter Dictionary -> LetterCount
; count the times l as the first letter in dict' words,
; and represent it in the form of LetterCount
(define (count-letter l dict)
  (make-letter-count l (starts-with# l dict)))

(check-expect (count-letter "z" '()) (make-letter-count "z" 0))
(check-expect (count-letter "a" dict1) (make-letter-count "a" 2))


; Dictionary -> LetterCount
; produces the Letter-Count for the letter
; that occurs most often as the first one in the given Dictionary dict.
(define (most-frequent dict)
  (query-most (count-by-letter LETTERS dict)))

(check-expect (most-frequent dict1) (make-letter-count "b" 3))

; List-of-letter-counts -> LetterCount
; query the most often letter in alolc
(define (query-most alolc)
  (cond [(empty? (rest alolc)) (first alolc)]
        [else (if (>= (letter-count-count (first alolc)) (letter-count-count (query-most (rest alolc))))
                  (first alolc)
                  (query-most (rest alolc)))]))

(check-expect (query-most (list (make-letter-count "b" 3)
                                (make-letter-count "u" 23)
                                (make-letter-count "z" 33))) 
              (make-letter-count "z" 33))


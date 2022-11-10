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
(define (count-by-letter alol dict)
  (cond [(empty? alol) '()]
        [else (cons (count-letter (first alol) dict)
                    (count-by-letter (rest alol) dict))]))


(check-expect (count-by-letter '() dict1) '())
(check-expect (count-by-letter letters1 dict1) (list (make-letter-count "a" 2)
                                                    (make-letter-count "b" 3)
                                                    (make-letter-count "d" 0)))

; Letter Dictionary -> LetterCount
(define (count-letter l dict)
  (make-letter-count l (starts-with# l dict)))

(check-expect (count-letter "z" '()) (make-letter-count "z" 0))
(check-expect (count-letter "a" dict1) (make-letter-count "a" 2))

; List-of-letter-counts -> List-of-letter-counts
; sort alolc use field count by descending order
(define (sort> alolc)
  (cond [(empty? alolc) '()]
        [else (insert (first alolc) (sort> (rest alolc)))]))

(check-expect (sort>(list (make-letter-count "a" 1)
                    (make-letter-count "b" 2)))
              (list (make-letter-count "b" 2)
                    (make-letter-count "a" 1)))
(check-expect (sort>(list (make-letter-count "a" 1)))
              (list (make-letter-count "a" 1)))
(check-expect '() '())

; LetterCount List-of-letter-counts -> List-of-letter-counts
; insert lc into alolc by descending order
(define (insert lc alolc)
  (cond [(empty? alolc) (list lc)]
        [else (if (larger? lc (first alolc))
                  (cons lc alolc)
                  (cons (first alolc) (insert lc (rest alolc))))]))

; LetterCount LetterCount -> Boolean;
; determine if lc1's count is larger than or equal to lc2's
(define (larger? lc1 lc2)
  (>= (letter-count-count lc1)
      (letter-count-count lc2)))

(check-expect (larger? (make-letter-count "b" 2) (make-letter-count "b" 2)) #true)
(check-expect (larger? (make-letter-count "b" 2) (make-letter-count "a" 1)) #true)
(check-expect (larger? (make-letter-count "a" 1) (make-letter-count "b" 2)) #false)





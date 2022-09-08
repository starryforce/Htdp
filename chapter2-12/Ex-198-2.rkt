#lang htdp/bsl+

(require 2htdp/batch-io)

; On OS X:
; (define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
(define LOCATION "C:/Users/StarryForce/OneDrive/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

(define dict0 '())
(define dict1 (list "apple" "bird" "cat" "egg" "fuck" "appear" "banana" "bad"))

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; A List-of-letters is one of:
; - '()
; - (cons Letter List-of-letters)

; A List-of-dictionarys is one of:
; - '()
; - (cons Dictionary List-of-dictionarys)

; Dictionary -> List-of-dictionarys
; consumes a Dictionary dic and produces a list of Dictionarys, one per Letter.
(define (words-by-first-letter dict)
  (cond [(empty? dict) '()]
        [else (combine-dict LETTERS dict)]))

(check-expect (words-by-first-letter dict0) '())
(check-expect (words-by-first-letter dict1)
              (list
               (list "apple" "appear")
               (list "bird" "banana" "bad")
               (list "cat")
               '()
               (list "egg")
               (list "fuck")
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()
               '()))

; List-of-letters Dictionary -> List-of-dictionarys
; generate a list of words starts with letter in alol seperately
(define (combine-dict alol dict)
  (cond [(empty? alol) '()]
        [else (cons (generate-dict (first alol) dict)
                    (combine-dict (rest alol) dict))]))

(check-expect (combine-dict (list "a" "b" "c") dict1)
              (list (list "apple" "appear")
                    (list "bird" "banana" "bad")
                    (list "cat")))

; Letter -> Dictionary
; generate a dictionary from Dictionary dict but all words in
; new dict start with the letter l
(define (generate-dict l dict)
  (cond [(empty? dict) '()]
        [else (if (starts-with l (first dict))
                  (cons (first dict) (generate-dict l (rest dict)))
                  (generate-dict l (rest dict)))]))

(check-expect (generate-dict "a" dict1) (list "apple" "appear"))
(check-expect (generate-dict "b" dict1) (list "bird" "banana" "bad"))
(check-expect (generate-dict "z" dict1) '())

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

; Dictionary -> LetterCount
; produces the Letter-Count for the letter
; that occurs most often as the first one in the given Dictionary dict.
(define (most-frequent.v2 dict)
  (make-lc (query-most (words-by-first-letter dict))))

(check-expect (most-frequent.v2 dict1) (make-letter-count "b" 3))


(define (make-lc dict)
  (make-letter-count (substring (first dict) 0 1) (length dict)))


; List-of-dictionarys -> Dictionary 
(define (query-most alod)
  (cond [(empty? alod) '()]
        [else (if (>= (length (first alod)) (length (query-most (rest alod))))
                  (first alod)
                  (query-most (rest alod)))]))

(check-expect (query-most (list (list "apple" "appear")
                                (list "bird" "banana" "bad")
                                (list "cat")))
              (list "bird" "banana" "bad"))


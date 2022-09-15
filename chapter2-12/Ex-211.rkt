#lang htdp/bsl+

(require 2htdp/batch-io)

; On OS X: 
; (define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
(define LOCATION "C:/Users/StarryForce/OneDrive/words")
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))
;(define AS-LIST (list "egg" "dog" "act" "cat"))

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

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))
 
; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low)
  (cond [(empty? low) '()]
        [else (cons (word->string (first low))
                    (words->strings (rest low)))]))

(check-expect (words->strings '()) '())
(check-expect (words->strings (list word0 word1)) (list "" "egg"))
(check-expect (words->strings (list word1 word2)) (list "egg" "dog"))
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
(define (in-dictionary los)
  (cond [(empty? los) '()]
        [else (if (in-dict? (first los))
                  (cons (first los) (in-dictionary (rest los)))
                  (in-dictionary (rest los)))]))

(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "egg" "geg")) (list "egg"))
(check-expect (in-dictionary (list "ffc" "tgy")) '())

; String -> Boolean
; determine if s is in the dictionary
(define (in-dict? s) (member? s AS-LIST))

(check-expect (in-dict? "egg") #true)
(check-expect (in-dict? "ffc") #false)
(check-expect (in-dict? "") #false)

; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements word)
  (list word))


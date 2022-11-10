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


; String -> Word
; converts s to the chosen word representation 
(define (string->word s) (explode s))
 
; Word -> String
; converts w to a string
(define (word->string w) (implode w))

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


; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
(define word0 '())
(define word1 (list "a"))
(define word2 (list "e" "d"))

; A List-of-words is one of:
; - '() or
; - (cons Word List-of-words)
; interpretation a List-of-words is a list of Word (also a list)

(define low0 '())
(define low1 (list '()))
(define low2 (list word1))
(define low3 (list word2))
(define low4 (list word1 word2))


; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
            (arrangements (rest w)))]))

(check-expect (arrangements word0) (list '()))
(check-expect (arrangements word2) (list word2 (list "d" "e")))

; 1String List-of-words -> List-of-words
; s inserted at the beginning, between all letters, and at the end of all words of alow.
(define (insert-everywhere/in-all-words s alow)
  (cond [(empty? alow) '()]
        [else (append (insert-everywhere/in-one-word s 0 (first alow))
                      (insert-everywhere/in-all-words s (rest alow)))]))

(check-expect (insert-everywhere/in-all-words "z" low0) '())
(check-expect (insert-everywhere/in-all-words "z" low1) (list (list "z")))
(check-expect (insert-everywhere/in-all-words "z" low2) (list (list "z" "a")
                                                              (list "a" "z")))
(check-expect (insert-everywhere/in-all-words "z" low3) (list (list "z" "e" "d")
                                                              (list "e" "z" "d")
                                                              (list "e" "d" "z")))
(check-expect (insert-everywhere/in-all-words "z" low4) (list (list "z" "a")
                                                              (list "a" "z")
                                                              (list "z" "e" "d")
                                                              (list "e" "z" "d")
                                                              (list "e" "d" "z")))

; 1String Number Word -> List-of-words
; s inserted at the beginning, between all letters, and at the end of the Word w.
(define (insert-everywhere/in-one-word s index w)
  (cond [(> index (length w)) '()]
        [else (cons (insert-at s index 0 w)
                    (insert-everywhere/in-one-word s (add1 index) w))]))

(check-expect (insert-everywhere/in-one-word "z" 0 word0) (list (list "z")))
(check-expect (insert-everywhere/in-one-word "z" 0 word1) (list (list "z" "a") (list "a" "z")))
(check-expect (insert-everywhere/in-one-word "z" 0 word2) (list (list "z" "e" "d")
                                                              (list "e" "z" "d")
                                                              (list "e" "d" "z")))



; Any Number Number List -> List
; insert s into position index in w
; contrait i is less than the length of w
(define (insert-at s i j w)
  (cond [(= i j) (cons s w)]
        [else (cons (first w)
                    (insert-at s i (add1 j) (rest w)))]))

(check-expect (insert-at "z" 0 0 '()) (list "z"))
(check-expect (insert-at "z" 0 0 (list "a")) (list "z" "a"))
(check-expect (insert-at "z" 1 0 (list "a")) (list "a" "z"))
(check-expect (insert-at "z" 1 0 (list "a" "b")) (list "a" "z" "b"))


#lang htdp/isl

(require 2htdp/batch-io)

; On OS X: 
; (define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
(define LOCATION "C:/Users/StarryForce/OneDrive/words")
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))
;(define AS-LIST (list "egg" "dog" "act" "cat"))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
(define (alternative-words s)
  (local (; String -> Word
          ; converts s to the chosen word representation 
          (define (string->word s) (explode s))
          (define alow (string->word s))
          
          ; Word -> List-of-words
          ; creates all rearrangements of the letters in w
          (define (arrangements w)
            (local (; 1String List-of-words -> List-of-words
                    ; s inserted at the beginning, between all letters, and at the end of all words of alow.
                    (define (insert-everywhere/in-all-words s alow)
                      (local (; 1String Number Word -> List-of-words
                              ; s inserted at the beginning, between all letters, and at the end of the Word w.
                              (define (insert-everywhere/in-one-word s index w)
                                (local (; Any Number Number List -> List
                                        ; insert s into position index in w
                                        ; contrait i is less than the length of w
                                        (define (insert-at s i j w)
                                          (cond [(= i j) (cons s w)]
                                                [else (cons (first w)
                                                            (insert-at s i (add1 j) (rest w)))])))
                                  (cond [(> index (length w)) '()]
                                        [else (cons (insert-at s index 0 w)
                                                    (insert-everywhere/in-one-word s (add1 index) w))]))))
                        (cond [(empty? alow) '()]
                              [else (append (insert-everywhere/in-one-word s 0 (first alow))
                                            (insert-everywhere/in-all-words s (rest alow)))]))))
              (cond
                [(empty? w) (list '())]
                [else (insert-everywhere/in-all-words (first w)
                                                      (arrangements (rest w)))])))
          (define arranged (arrangements alow))

          ; List-of-words -> List-of-strings
          ; turns all Words in low into Strings 
          (define (words->strings low)
            (local (; Word -> String
                    ; converts w to a string
                    (define (word->string w) (implode w)))
              (cond [(empty? low) '()]
                    [else (cons (word->string (first low))
                                (words->strings (rest low)))])))
          (define transformed (words->strings arranged))
          
          ; List-of-strings -> List-of-strings
          ; picks out all those Strings that occur in the dictionary 
          (define (in-dictionary los)
            (local (; String -> Boolean
                    ; determine if s is in the dictionary
                    (define (in-dict? s) (member? s AS-LIST))
                    )
              (cond [(empty? los) '()]
                    [else (if (in-dict? (first los))
                              (cons (first los) (in-dictionary (rest los)))
                              (in-dictionary (rest los)))])))
          (define result (in-dictionary transformed)))
    ; - IN -
    result))
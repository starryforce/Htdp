#lang htdp/isl+

#| Exercise 453.
Design the function tokenize.
It turns a Line into a list of tokens.
Here a token is either a 1String or a String that consists of lower-case letters and nothing else.
That is, all white-space 1Strings are dropped;
all other non-letters remain as is;
and all consecutive letters are bundled into “words.”
Hint Read up on the string-whitespace? function.
|#

; A 1String is a String of length 1, 
; including
; – "\\" (the backslash),
; – " " (the space bar), 
; – "\t" (tab),
; – "\r" (return), and 
; – "\b" (backspace).
; interpretation represents keys on the keyboard

; A Line is a [List-of 1String].
(define ex1 '("e" "g" "g" " " "b" "e" "\\" "\t" "n" "i" "c" "e"))


; Token is one of:
; - 1String
; - String (consists of lower-case letters and nothing else)

; Line -> [List-of Token]
; turns a Line into a list of tokens
(define (tokenize l)
  (cond [(empty? l) '()]
        [(andmap string-whitespace? l) '()]
        [else (cons (first-token l)
                    (tokenize (remove-first-token l)))]))

(check-expect (tokenize '()) '())
(check-expect (tokenize '(" ")) '())
(check-expect (tokenize ex1) '("egg"
                               "be"
                               "\\"
                               "nice"))

; Line -> [Maybe String | 1String]
; retrieves the first token in l
(define (first-token l)
  (cond [(string-whitespace? (first l)) (first-token (rest l))]
        [(string-alphabetic? (first l)) (if (and (cons? (rest l)) (string-alphabetic? (second l)))
                                            (string-append (first l) (first-token (rest l)))
                                            (first l))]
        [else (first l)]))

(check-expect (first-token '("\\" "a" "b" "c")) "\\")
(check-expect (first-token '(" " "a" "b" "c")) "abc")
(check-expect (first-token '("a" "b" "c" " ")) "abc")
(check-expect (first-token '("a" "b" "c")) "abc")
(check-expect (first-token '("a" "b" "c" "\\")) "abc")


; Line -> Line
; removes first token in l
(define (remove-first-token l)
  (cond [(string-whitespace? (first l)) (remove-first-token (rest l))]
        [(string-alphabetic? (first l)) (if (and (cons? (rest l)) (string-alphabetic? (second l)))
                                            (remove-first-token (rest l))
                                            (rest l))]
        [else (rest l)]))

(check-expect (remove-first-token '("\\" "a" "b" "c")) '("a" "b" "c"))
(check-expect (remove-first-token '(" " "a" "b" "c")) '())
(check-expect (remove-first-token '("a" "b" "c" " ")) '(" "))
(check-expect (remove-first-token '("a" "b" "c")) '())
(check-expect (remove-first-token '("a" "b" "c" "\\")) '("\\"))
#lang htdp/isl+

#| Exercise 422.
Define the function list->chunks.
It consumes a list l of arbitrary data and a natural number n.
The function’s result is a list of list chunks of size n.
Each chunk represents a sub-sequence of items in l.

Use list->chunks to define bundle via function composition. 
|#

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; [List-of Any] N -> [List-of [List-of Any]]
; generate a list of list chunks of size n.
; Each chunk represents a sub-sequence of items in l.
(define (list->chunks l n)
  (cond [(empty? l) '()]
        [else (cons (take l n) (list->chunks (drop l n) n))]))

(check-expect (list->chunks '() 2) '())
(check-expect (list->chunks '(1 "abc") 4) '((1 "abc")))
(check-expect (list->chunks '(1 "abc" #t 42 2 "hello") 2)
              '((1 "abc")
                (#t 42)
                (2 "hello")))
(check-expect (list->chunks '(1 "abc" #t 42 2 "hello") 4)
              '((1 "abc" #t 42)
                (2 "hello")))
                          

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))


; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
(define (bundle s n)
  (map implode (list->chunks s n)))

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())

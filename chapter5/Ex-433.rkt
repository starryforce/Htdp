#lang htdp/isl+

#| Exercise 433.
Develop a checked version of bundle that is guaranteed to terminate for all inputs.
It may signal an error for those cases where the original version loops.
|#

(define TOO_SMALL "chunk size should be at least 1")

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
; termination when n equals to 0, drop always generate same result with input,
; the evaluation never terminate.
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (if (> n 0)
         (cons (implode (take s n)) (bundle (drop s n) n))
         (error TOO_SMALL))]))
 
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

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-error (bundle '("a" "b") 0) TOO_SMALL)
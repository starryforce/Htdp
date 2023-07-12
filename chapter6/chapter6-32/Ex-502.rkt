#lang htdp/isl+



(check-expect
 (palindrome (explode "abc")) (explode "abcba"))

(define (palindrome s0)
  (local (; [NEList-of 1String] ??? -> [NEList-of 1String]
          ; creates a palindrome from s0
          ; accumulator a is the reverse list of items in s lack from s0
          (define (palindrome/a s a)
            (cond [(empty? (rest s)) (append s0 a)]
                  [else (palindrome/a (rest s) (cons (first s) a))])))
    (palindrome/a s0 '())))
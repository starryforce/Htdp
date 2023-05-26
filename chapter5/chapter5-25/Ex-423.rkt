#lang htdp/isl+

#| Exercise 423.
Define partition. It consumes a String s and a natural number n.
The function produces a list of string chunks of size n.

For non-empty strings s and positive natural numbers n,
(equal? (partition s n) (bundle (explode s) n))

is #true. But don’t use this equality as the definition for partition; use substring instead.
Hint Have partition produce its natural result for the empty string.
For the case where n is 0, see exercise 421.

Note The partition function is somewhat closer to
what a cooperative DrRacket environment would need than bundle.
|#

; String N ->[List-of String]
(define (partition s n)
  (cond [(= (string-length s) 0) '()]
        [(< (string-length s) n) (list s)]
        [else (cons (substring s 0 n)
                    (partition (substring s n) n))]))

(check-expect (partition "" 2) '())
(check-expect (partition "abcd" 5) '("abcd"))
(check-expect (partition "abcd" 2) '("ab" "cd"))
(check-expect (partition "abcd" 3) '("abc" "d"))
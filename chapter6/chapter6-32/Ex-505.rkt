#lang htdp/isl+

#| Exercise 505.
Design the function is-prime,
which consumes a natural number and
returns #true if it is prime and #false otherwise.
|#

; N [>=1] -> Boolean
; determines whether n is a prime number
(check-expect (is-prime? 7) #t)
(check-expect (is-prime? 4) #f)
(check-expect (is-prime? 439) #t)

(define (is-prime? n0)
  (local (; N N -> Boolean
          ; determines whether n is a prime number
          (define (is-prime?/a guess)
            (cond
              [(= guess 1) #t]
              [else (if (= 0 (remainder n0 guess))
                        #f
                        (is-prime?/a (sub1 guess)))])))
    (is-prime?/a (sub1 n0))))

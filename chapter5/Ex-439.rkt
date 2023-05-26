#lang htdp/isl+

#| Exercise 439.
Copy gcd-structural into DrRacket and evaluate
(time (gcd-structural 101135853 45014640))
in the interactions area.
|#

(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

(time (gcd-structural 101135853 45014640))
; cpu time: 1343 real time: 2148 gc time: 375
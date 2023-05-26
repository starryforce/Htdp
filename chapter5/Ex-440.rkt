#lang htdp/isl+

#| Exercise 440.
Copy gcd-generative into the definitions area of DrRacket and evaluate
(time (gcd-generative 101135853 45014640))
in the interactions area. 
|#

(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S)) 
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

(time (gcd-generative 101135853 45014640))
; cpu time: 0 real time: 0 gc time: 0
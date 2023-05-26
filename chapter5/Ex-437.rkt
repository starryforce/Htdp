#lang htdp/isl+

#| Exercise 437.
Define solve and combine-solutions so that
- special computes the length of its input,
- special negates each number on the given list of numbers, and
- special uppercases the given list of strings.
What do you conclude from these exercises? 
|#

; [List-of Any] -> Number
; computes the length of P
(define (special-1 P)
  (local (; [List-of Any] -> Number
          (define (solve p) 0)
          ; [List-of Any] Number -> Number
          (define (combine-solutions _ count) (add1 count)))
    (cond
      [(empty? P) (solve P)]
      [else
       (combine-solutions
        P
        (special-1 (rest P)))])))

(check-expect (special-1 '()) 0)
(check-expect (special-1 '(1 2)) 2)

; [List-of Number] -> [List-of Number]
; negates each number on P
(define (special-2 P)
  (local ((define (solve p) '())
          ; [List-of Number] -> [List-of Number]
          (define (combine-solutions p other)
            (cons (- (first p)) other)))
    (cond
      [(empty? P) (solve P)]
      [else
       (combine-solutions
        P
        (special-2 (rest P)))])))

(check-expect (special-2 '()) '())
(check-expect (special-2 '(1 -3 2)) '(-1 3 -2))


; [List-of String] -> [List-of String]
; uppercases strings in P
(define (special-3 P)
  (local ((define (solve p) '())
          (define (combine-solutions p other)
            (cons (string-upcase (first p)) other)))
    (cond
      [(empty? P) (solve P)]
      [else
       (combine-solutions
        P
        (special-3 (rest P)))])))

(check-expect (special-3 '()) '())
(check-expect (special-3 '("hello" "world")) '("HELLO" "WORLD"))

; structural recursion is just a special condition of generative recursion

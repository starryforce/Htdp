#lang htdp/isl+

#|
Exercise 318. Design depth.
The function consumes an S-expression and determines its depth.
An Atom has a depth of 1.
The depth of a list of S-expressions is the maximum depth of its items plus 1. 
|#



; S-expr -> N
; determines sexp's depth
(define (depth sexp)
  (local ((define (atom? s)
            (or (string? s)
                (number? s)
                (symbol? s)))
          ; SL -> N
          ; determine sl's depth
          (define (depth-sl sl)
            (cond [(empty? sl) 1]
                  [else (max (+ 1 (depth (first sl)))
                             (depth-sl (rest sl)))]))

          ; Atom -> N
          ; determine atom's depth
          (define (depth-atom at)
            (cond [(number? at) 1]
                  [(string? at) 1]
                  [(symbol? at) 1])))
    (cond [(atom? sexp) (depth-atom sexp)]
          [else (depth-sl sexp)])))



(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello) hello)) 4)

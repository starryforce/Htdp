#lang htdp/isl+

#|
Exercise 319. Design substitute.
It consumes an S-expression s and two symbols, old and new.
The result is like s with all occurrences of old replaced by new. 
|#

(define (main sexp old new)
  (local ((define (atom? s)
            (or (string? s)
                (number? s)
                (symbol? s)))
          ; S-expr Symbol Symbol -> S-expr
          ; replace all Symbol old with new in sexp
          (define (substitute sexp)
            (cond [(atom? sexp) (substitute-atom sexp)]
                  [else (substitute-sl sexp)]))

          ; SL Symbol Symbol -> SL
          ; replace all Symbol old with new in sl
          (define (substitute-sl sl)
            (cond [(empty? sl) '()]
                  [else (cons (substitute (first sl)) (substitute-sl (rest sl)))]))

          ; Atom Symbol Symbol -> Atom
          ; replace all Symbol old with new in at
          (define (substitute-atom at)
            (cond [(number? at) at]
                  [(string? at) at]
                  [(symbol? at) (if (symbol=? at old) new at)])))
    (substitute sexp)))




(check-expect (main 'world 'world 'hello) 'hello)
(check-expect (main '(world hello) 'world 'hello) '(hello hello))
(check-expect (main '(((world) hello) hello) 'world 'hello) '(((hello) hello) hello))

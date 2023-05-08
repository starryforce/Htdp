#lang htdp/isl+

(define (atom? s)
  (or (string? s)
      (number? s)
      (symbol? s)))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute 'world 'world 'hello) 'hello)
(check-expect (substitute '(world hello) 'world 'hello) '(hello hello))
(check-expect (substitute '(((world) hello) hello) 'world 'hello) '(((hello) hello) hello))
 
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))

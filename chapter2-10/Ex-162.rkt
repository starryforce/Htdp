#lang htdp/bsl

(define WAGE_PER_HOUR 14)

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (cond [(<= h 100) (* WAGE_PER_HOUR h)]
        [else (error "one of the item exceeds 100")]))

(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons (* 28 WAGE_PER_HOUR) '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons (* 4 WAGE_PER_HOUR) (cons (* 2 WAGE_PER_HOUR) '())))
(check-error (wage* (cons 102 (cons 2 '()))) "one of the item exceeds 100")
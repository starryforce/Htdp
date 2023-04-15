#lang htdp/isl+

(require 2htdp/abstraction)

(define input '(("a") ("a" "b") ("a" "b" "c")))
(define output '(1 2 3))

; [List-of [List-of String]] -> [List-of Number]
(define (words-on-line allos)
  (for/list ([alos allos]) (length alos)))


(check-expect (words-on-line input) output)

(define (words-on-line-1 allos)
  (match allos
    ['() '()]
    [(cons head tail)
     (cons (length head) (words-on-line-1 tail))]))

(check-expect (words-on-line-1 input) output)
#lang htdp/isl+

(require 2htdp/abstraction)

; Number -> [List-of Number]
; creates the list (list 0 ... (- n 1)) for any natural number n;
(define (fn1 n)
  (for/list ([i n]) i))

(check-expect (fn1 3) (list 0 1 2))


; Number -> [List-of Number]
; creates the list (list 1 ... n) for any natural number n;
(define (fn2 n)
  (for/list ([i n]) (add1 i)))

(check-expect (fn2 3) (list 1 2 3))

; Number -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n;
(define (fn3 n)
  (for/list ([i n]) (/ 1 (add1 i))))
(check-expect (fn3 3) (list 1 1/2 1/3))

; Number -> [List-of Number]
; creates the list of the first n even numbers
(define (fn4 n)
  (for/list ([i n])
    (* 2 i)))

(check-expect (fn4 3) (list 0 2 4))

; Number -> [List-of [List-of Number]]
; creates a diagonal square of 0s and 1s
(define (fn5 n)
  (for/list ([i n])
    (for/list ([j n]) (if (= i j) 1 0))))


(check-expect (fn5 1) (list (list 1)))
(check-expect (fn5 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

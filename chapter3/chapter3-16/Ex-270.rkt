#lang htdp/isl

; Number -> [List-of Number]
; creates the list (list 0 ... (- n 1)) for any natural number n;
(define (fn1 n)
  (local (; Number -> Number
          ; keep same
          (define (fn i) i))
    (build-list n fn)))

(check-expect (fn1 3) (list 0 1 2))

; Number -> [List-of Number]
; creates the list (list 1 ... n) for any natural number n;
(define (fn2 n)
  (local (; Number -> Number
          ; add 1 to i
          (define (fn i) (add1 i)))
    (build-list n fn)))

(check-expect (fn2 3) (list 1 2 3))


; Number -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n;
(define (fn3 n)
  (local (; Number -> Number
          ; get 1/i from i
          (define (fn n) (/ 1 (add1 n))))
    (build-list n fn)))

(check-expect (fn3 3) (list 1 1/2 1/3))


; Number -> [List-of Number]
; creates the list of the first n even numbers
(define (fn4 n)
  (local (; Number -> Number
          ; get even number
          (define (fn n) (* 2 n)))
    (build-list n fn)))

(check-expect (fn4 3) (list 0 2 4))


; Number -> [List-of [List-of Number]]
; creates diagonal squares of 0s and 1s:
(define (fn5 n)
  (local (; Number -> [List-of Number]
          ; generate one row
           (define (make-row row-index)
            (local (; Number -> Number
                    ; determine item in row is 0 or 1
                    (define (make-item item-index)
                      (if (= row-index item-index) 1 0)))
              (build-list n make-item))))
    (build-list n make-row)))

(check-expect (fn5 1) (list (list 1)))
(check-expect (fn5 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; Number [Number -> Number]
; -> [List-of Number]
; ; tabulates f between n 
; and 0 (incl.) in a list
(define (tabulate n f) (build-list n f))

(define (tab-sin-from-abstract n)
  (tabulate n sin))

(define delta 0.001)
(check-within (tab-sin-from-abstract 4) (tab-sin-from-abstract 4) delta)
(check-within (tab-sin-from-abstract 2) (tab-sin-from-abstract 2) delta)
(check-within (tab-sin-from-abstract 0) (tab-sin-from-abstract 0) delta)

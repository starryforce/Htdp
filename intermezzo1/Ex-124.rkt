#lang htdp/bsl

; (define PRICE 5)
; (define SALES-TAX (* 0.08 PRICE))
; (define TOTAL (+ PRICE SALES-TAX))


; (define COLD-F 32)
; (define COLD-C (fahrenheit->celsius COLD-F))
; (define (fahrenheit->celsius f)
;  (* 5/9 (- f 32)))

(define LEFT -100)
(define RIGHT 100)
(define (f x) (+ (* 5 (expt x 2)) 10))
(define f@LEFT (f LEFT))
(define f@RIGHT (f RIGHT))
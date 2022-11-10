#lang htdp/bsl

(define TAX-FREE 1000)
(define TAX-MORE 10000)
(define CLASS-1 0.05)
(define CLASS-2 0.08)

; A Price falls into one of three intervals: 
; — 0 through 1000
; — 1000 through 10000
; — 10000 and above.
; interpretation the price of an item


; Price -> Number
; computes the amount of tax charged for p
(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p TAX-FREE)) 0]
    [(and (<= TAX-FREE p) (< p TAX-MORE)) (* CLASS-1 p)]
    [(>= p TAX-MORE) (* CLASS-2 p)]))

(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* CLASS-1 1000))
(check-expect (sales-tax 1282) (* CLASS-1 1282))
(check-expect (sales-tax 10000) (* CLASS-2 10000))
(check-expect (sales-tax 12017) (* CLASS-2 12017))
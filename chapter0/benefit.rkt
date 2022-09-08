#lang htdp/bsl+

(define amount 10000)
(define rate_by_year 0.024189)
(define month 24)
(define rate_by_month (/ rate_by_year 12))
(define pay_by_month (/ amount month))


(define amount-list (range 1896 79 -79))


(define (sum-amount l)
  (cond [(empty? l) 0]
        [else (+ (calc (first l))
                 (sum-amount (rest l)))]))


(define (calc money)
  (* money rate_by_month))


(sum-amount amount-list)
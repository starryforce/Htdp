#lang htdp/bsl

(define (==> sunny friday)
  (and (boolean=? sunny #false) (boolean=? friday #true)))

(==> #true #true)
(==> #true #false)
(==> #false #false)

; should be true
(==> #false #true)
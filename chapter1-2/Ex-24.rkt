#lang htdp/bsl


(define (==> x y)
  (or (not x) y))

(==> #true #false)

;(or (not #true) #false)
;(or #false #false)
;#false

#lang htdp/isl+

(require 2htdp/abstraction)

(define ex (list "a" "b" "c"))

; [List-of ITEM] -> [List-of [List Number ITEM]]
; produces a list of the same items paired with their relative index from l
(define (enumerate l)
  (local ((define indexs (build-list (length l) (lambda (i) (add1 i))))
          )
    (map list indexs l)))

(check-expect (enumerate ex) (list (list 1 "a") (list 2 "b") (list 3 "c")))





  
#lang htdp/isl+

(require 2htdp/abstraction)

(define ex1 (list 1 2 3))
(define ex2 (list "a" "b" "c"))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generates all pairs of items from l1 and l2
(define (cross l1 l2)
  (for*/list ([x l1] [y l2])
    (list x y)))

(check-expect (cross ex1 ex2)
              (list (list 1 "a")
                    (list 1 "b")
                    (list 1 "c")
                    (list 2 "a")
                    (list 2 "b")
                    (list 2 "c")
                    (list 3 "a")
                    (list 3 "b")
                    (list 3 "c")))

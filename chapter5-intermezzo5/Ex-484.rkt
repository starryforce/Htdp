#lang htdp/isl+

; Exercise 484

(define (infL l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (local ((define s (infL (rest l))))
            (if (< (first l) s) (first l) s))]))

(infL (list 3 2 1 0))

; ==
(local ((define s (infL (list 2 1 0))))
            (if (< 3 s) 3 s))
; ==
(local ((define s (infL (list 1 0))))
            (if (< 2 s) 2 s))
; ==
(local ((define s (infL (list 0))))
            (if (< 1 s) 1 s))
; ==
(local ((define s 0))
            (if (< 1 s) 1 s))
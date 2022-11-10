#lang htdp/bsl+


(define ex1 (list (string=? "a" "b") #false))
(define ex2 (list (+ 10 20) (* 10 20) (/ 10 20)))
(define ex3 (list "dana" "jane" "mary" "laura"))

(check-expect ex1 (list #false #false))
(check-expect ex2 (list 30 200 1/2))
(check-expect ex3 (list "dana" "jane" "mary" "laura"))
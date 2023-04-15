#lang htdp/isl+

(require 2htdp/abstraction)

(define RATE 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts
(define (convert-euro l)
  (for/list ([amount l])
    (* amount RATE)))


(check-expect (convert-euro (list 1 10 100)) (list 1.06 10.6 106))
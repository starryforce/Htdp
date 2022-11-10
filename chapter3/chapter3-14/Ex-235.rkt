#lang htdp/isl

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> Boolean
; does l contains "atom"
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; does l contains "basic"
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; does l contains "zoo"
(define (contains-zoo? l)
  (contains? "zoo" l))
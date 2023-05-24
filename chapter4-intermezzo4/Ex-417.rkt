#lang racket
;#lang htdp/isl+

#| Exercise 417.
Evaluate (expt 1.001 1e-12) in Racket and in ISL+. Explain what you see.
|#

; racket
; 1.000000000000001
; Plain Racket interprets all decimal numbers as inexact numbers; it also renders all real numbers as decimals

; isl+
; #i1.000000000000001
; out of range, so isl+ use an appoprate
#lang htdp/isl+

; Exercise 368.
; Formulate a data definition that replaces the informal “or” signature for the definition of the list-of-attributes? function.

; Sub -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; A Sub is one of:
; - [List-of Attribute]
; Xexpr.v2
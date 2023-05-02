#lang htdp/isl+

#| Exercise 371.
Refine the definition of Xexpr so that you can represent XML elements,
including items in enumerations, that are plain strings.
|#

; An XWord is '(word ((text String))).

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; A Body is one of:
; - XWord
; - [List-of Xexpr.v3]

; An Xexpr.v3 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
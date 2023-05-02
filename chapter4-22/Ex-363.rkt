#lang htdp/isl+

#|  Exercise 363.
All elements of Xexpr.v2 start with a Symbol,
but some are followed by a list of attributes and some by just a list of Xexpr.v2s.
Reformulate the definition of Xexpr.v2 to isolate the common beginning and highlight the different kinds of endings.

Eliminate the use of List-of from Xexpr.v2. 
|#

; An Xexpr.v3 is a list (cons Symbol Content)
; where Content is one of:
; - Body
; - (cons [List-of Attribute] Body)
; where Body is short for [List-of Xexpr.v3]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

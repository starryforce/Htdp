#lang htdp/isl+

#| Exercise 369.
Design find-attr. The function consumes a list of attributes and a symbol.
If the attributes list associates the symbol with a string,
the function retrieves this string; otherwise it returns #false.
Look up assq and use it to define the function.
|#

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a1 '())
(define a2 '((color "red") (size "small") (weight "normal")))

; [List-of Attribute] Symbol -> [Maybe String]
; retrieves the value from aloa that associate with s
(define (find-attr aloa s)
  (local ((define item (assq s aloa)))
    (if (false? item) #false (second item) )))

(check-expect (find-attr a1 'd) #false)
(check-expect (find-attr a2 'size) "small")
(check-expect (find-attr a2 'opacity) #false)
#lang htdp/isl+

#| Exercise 387.
Design cross.
The function consumes a list of symbols and a list of numbers and
produces all possible ordered pairs of symbols and numbers.
That is, when given '(a b c) and '(1 2),
the expected result is '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)).
|#

; [List-of Symbol] [List-of Number] -> [List-of '(Symbol Number)]
; produces all possible ordered pairs of symbols and numbers
(define (cross alos alon)
  (cond [(empty? alos) '()]
        [else (append (attach (first alos) alon) 
                      (cross (rest alos) alon) )]))

(check-expect (cross '() '(1 2)) '())
(check-expect (cross '(a b c) '()) '())
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))


; Symbol [List-of Number] -> [List-of '(Symbol Number)]
; produces all possible ordered pairs with Symbol is s
(define (attach s alon)
  (cond [(empty? alon) '()]
        [else (cons (list s (first alon))
                    (attach s (rest alon)))]))

(check-expect (attach 'a '()) '())
(check-expect (attach 'a '(1 2)) '((a 1) (a 2)))


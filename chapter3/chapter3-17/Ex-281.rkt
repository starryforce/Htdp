#lang htdp/isl+

(require 2htdp/image)

; consumes a number and decides whether it is less than 10;
(lambda (x) (< x 10))

; multiplies two given numbers and turns the result into a string;
(lambda (x y) (number->string (* x y)))

; consumes a natural number and returns 0 for evens and 1 for odds;
(lambda (n) (modulo n 2))

; consumes two inventory records and compares them by price; and
(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)
(define ex1 (make-IR "apple" 10))
(define ex2 (make-IR "banana" 20))
(lambda (x y) (> (IR-price x) (IR-price y)))

; adds a red dot at a given Posn to a given Image.
(lambda (p bg) (place-image (circle 1 "solid" "red")
                            (posn-x p)
                            (posn-y p)
                            bg))
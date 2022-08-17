#lang htdp/bsl

(require 2htdp/image)

(define ex1 (rectangle 100 100 "solid" "red"))
(define ex2 (rectangle 50 50 "solid" "red"))
(define ex3 (rectangle 100 50 "solid" "red"))
(define ex4 (circle 100 "solid" "red"))
(define ex5 (circle 50 "solid" "red"))

; A List-of-images is one of:
; - '()
; - (cons Image List-of-images)
(define ex11 '())
(define ex12 (cons ex1 '()))
(define ex13 (cons ex2 (cons ex4 '())))

; ImageOrFalse is one of:
; – Image
; – #false

; List-of-images Number -> ImageOrFalse
(define (ill-sized? loi n)
  (cond [(empty? loi) #false]
        [else (cond [(meet-size? (first loi) n) (first loi)]
                    [else (ill-sized? (rest loi) n)])]))

(check-expect (ill-sized? ex11 50) #false)
(check-expect (ill-sized? ex12 100) ex1)
(check-expect (ill-sized? ex12 50) #false)
(check-expect (ill-sized? ex13 70) #false)
(check-expect (ill-sized? ex13 50) ex2)
(check-expect (ill-sized? ex13 200) ex4)

; Image Number -> Boolean
; check if i is an image with both width and height are size;
(define (meet-size? i size)
  (and (= (image-width i) size)
       (= (image-height i) size)))

(check-expect (meet-size? ex1 50) #false)
(check-expect (meet-size? ex1 100) #true)
(check-expect (meet-size? ex3 70) #false)
(check-expect (meet-size? ex3 50) #false)
(check-expect (meet-size? ex3 100) #false)
(check-expect (meet-size? ex4 200) #true)
(check-expect (meet-size? ex4 50) #false)


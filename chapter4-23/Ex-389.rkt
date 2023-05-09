#lang htdp/isl+

#| Exercise 389. 
Design the zip function, which consumes a list of names,
represented as strings, and a list of phone numbers, also strings.
It combines those equally long lists into a list of phone records:
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)
Assume that the corresponding list items belong to the same person. 
|#

(define ex-names0 '())
(define ex-names1 '("Tom"))
(define ex-names2 '("Tom" "Jerry"))
(define ex-phones0 '())
(define ex-phones1 '("001"))
(define ex-phones2 '("001" "002"))

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; combine related name and phone number into one record
; and get the record list
(define (zip alona alonu)
  (cond [(empty? alona) '()]
        [else (cons (make-phone-record (first alona) (first alonu))
                    (zip (rest alona) (rest alonu)))]))

(check-expect (zip ex-names0 ex-phones0) '())
(check-expect (zip ex-names1 ex-phones1) (list (make-phone-record "Tom" "001")))
(check-expect (zip ex-names2 ex-phones2) (list (make-phone-record "Tom" "001")
                                               (make-phone-record "Jerry" "002")))

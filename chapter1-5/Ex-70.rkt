#lang htdp/bsl

#|
(define-struct centry [name home office cell])

(= (centry-name (make-centry 1 2 3 4))  1)
(= (centry-home (make-centry 1 2 3 4))  2)
(= (centry-office (make-centry 1 2 3 4))  3)
(= (centry-cell (make-centry 1 2 3 4))  4)


(define-struct phone [area number])

(= (phone-area (make-phone 1 2)) 1)
(= (phone-number (make-phone 1 2)) 2)
|#

(define-struct centry [name home office cell])
(define-struct phone [area number])

(phone-area
 (centry-office
  (make-centry "Shriram Fisler"
    (make-phone 207 "363-2421")
    (make-phone 101 "776-1099")
    (make-phone 208 "112-9981"))))
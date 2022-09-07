#lang htdp/bsl


(define-struct point [x y z])
(define-struct none  [])

; yes, cater to the point
(make-point 1 2 3)

; yes, also
(make-point (make-point 1 2 3) 4 5)

; yes
(make-point (+ 1 2) 3 4)

; no
(make-none)

; yes
(make-point 1 4 5)
(make-point (point-x (make-point 1 2 3)) 4 5)
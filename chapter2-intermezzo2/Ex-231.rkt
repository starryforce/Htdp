#lang htdp/bsl+

(define q1 '(1 "a" 2 #false 3 "c"))
(define a10 (list '1 '"a" '2 '#false '3 '"c"))
(define a11 (list 1 "a" 2 #false 3 "c"))
(define a12 (cons 1 (cons "a" (cons 2 (cons #false (cons 3 (cons "c" '())))))))

(check-expect q1 a10)
(check-expect q1 a11)
(check-expect q1 a12)

(define q2 '())
(define a20 (list))

(check-expect q2 a20)

(define q3 '(("alan" 1000)
             ("barb" 2000)
             ("carl" 1500)))

(define a30 (list '("alan" 1000)
                 '("barb" 2000)
                 '("carl" 1500)))
(define a31 (list (list '"alan" '1000)
                  (list '"barb" '2000)
                  (list '"carl" 1500)))
(define a32 (list (list "alan" 1000)
                  (list "barb" 2000)
                  (list "carl" 1500)))
(define a33 (cons (cons "alan" (cons 1000 '())) (cons (cons "barb" (cons 2000 '())) (cons (cons "carl" (cons 1500 '())) '()))))
             
(check-expect q3 a30)
(check-expect q3 a31)
(check-expect q3 a32)
(check-expect q3 a33)
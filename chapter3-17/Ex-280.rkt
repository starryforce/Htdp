#lang htdp/isl+

; 1. 3
((lambda (x y) (+ x (* x y)))
 1 2)

; 2. 14
((lambda (x y)
   (+ x
      (local ((define z (* y y)))
        (+ (* 3 z) (/ 1 x)))))
 1 2)

; 3. 13.25
((lambda (x y)
   (+ x
      ((lambda (z)
         (+ (* 3 z) (/ 1 z)))
       (* y y))))
 1 2)
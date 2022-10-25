#lang htdp/isl+

((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)

#|
((local ((define (f_0 x) (+ x 3))
         (define (g_0 x) (* x 4)))
   (if (odd? (f_0 (g_0 1)))
       f_0
       g_0))
 2)

Step.1
(define (f_0 x) (+ x 3))
(define (g_0 x) (* x 4))
((if (odd? (f_0 (g_0 1)))
       f_0
       g_0))
 2)

Step.2
((if (odd? (+ 4 3))
       f_0
       g_0))
 2)

Step.3
((if (odd? 7)
       f_0
       g_0))
 2)

Step.4
((if #true
       f_0
       g_0))
 2)

Step.5
(f_0 2)

Step.6
(+ 2 3)

Step.7
5

|#
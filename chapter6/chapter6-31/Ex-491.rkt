#lang htdp/isl+

; Exercise 491.

; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute.v2 '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute.v2 l0)
  (local (
    ; [List-of Number] Number -> [List-of Number]
    (define (relative->absolute/a l accu-dist)
      (cond
        [(empty? l) '()]
        [else
          (local ((define accu (+ (first l) accu-dist)))
            (cons accu
                 (relative->absolute/a (rest l) accu)))])))
    (relative->absolute/a l0 0)))

(define (relative->absolute l)
 (reverse
   (foldr (lambda (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse (rest l)))))

(define SIZE 5000)

(time (relative->absolute.v2 (build-list SIZE add1)))
(time (relative->absolute (build-list SIZE add1)))
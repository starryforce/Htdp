#lang htdp/isl+

; version 4
; [X Y] [X Y -> Y] Y [List-of X] -> Y
(define (f*ldl f e l0)
  (local (; Y [List-of X] -> Y
          ; accumulator a is the result of items on l lacks from l0
          (define (fold/a a l)
            (cond
              [(empty? l) a]
              [else
               (fold/a (f (first l) a) (rest l))])))
    (fold/a e l0)))

; (f e (f e (f e x1)))



; N [N -> X] -> [List-of X]
; build a list that length is (sub1 n)
(define (build-l*st n0 f)
  (local ((define (build-list/a n a)
            (cond [(zero? n) a]
                  [else (build-list/a (sub1 n) (cons (f (sub1 n)) a))])))
    (build-list/a n0 '())))
(check-expect (build-l*st 10 add1) (build-list 10 add1))
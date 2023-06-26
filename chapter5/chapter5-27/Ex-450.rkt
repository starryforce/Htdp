#lang htdp/isl+

#| Exercise 450.
A function f is monotonically increasing if (<= (f a) (f b)) holds whenever (< a b) holds.
Simplify find-root assuming the given function is not only continuous but also monotonically increasing.
|#

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption

(define ε 0.000001)

(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(<= 0 f@mid)
           (find-root f left mid)]
          [(<= f@mid 0)
           (find-root f mid right)]))]))


; Number -> Number
(define (poly x)
  (+ x 1))

(check-satisfied (find-root poly -100 100) (lambda (x) (= (poly (round x)) 0)))
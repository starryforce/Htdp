#lang htdp/isl+

#| Exercise 449.
As presented in figure 159,
find-root computes the value of f for each boundary value twice to generate the next interval.
Use local to avoid this recomputation.

In addition, find-root recomputes the value of a boundary across recursive calls.
For example, (find-root f left right) computes (f left) and,
if [left,mid] is chosen as the next interval, find-root computes (f left) again.
Introduce a helper function that is like find-root but consumes not only left and right
but also (f left) and (f right) at each recursive stage.

How many recomputations of (f left) does this design maximally avoid?
Note The two additional arguments to this helper function change at each recursive stage,
but the change is related to the change in the numeric arguments.
These arguments are so-called accumulators, which are the topic of Accumulators. 
|#
(define ε 0.000001)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption 
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define left-value (f left))
             (define right-value (f right)))
       (find-root-inner f left right left-value right-value))]))


(define (find-root-inner f left right left-value right-value)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= left-value 0 f@mid) (<= f@mid 0 left-value))
          (find-root-inner f left mid left-value f@mid)]
         [(or (<= f@mid 0 right-value) (<= right-value 0 f@mid))
          (find-root-inner f mid right f@mid right-value)]))]))



; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(check-satisfied (find-root poly 3 1000) (lambda (x) (= (poly (round x)) 0)))

; 2 * recursive times - 1

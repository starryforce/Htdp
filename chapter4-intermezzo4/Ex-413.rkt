#lang htdp/isl+

#| Exercise 413.
Design inex*.
The function multiplies two Inex representations of numbers,
including inputs that force an additional increase of the output’s exponent.
Like inex+, it must signal its own error if the result is out of range,
not rely on create-inex to perform error checking.

|#

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
      10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

; Inex Inex -> Inex
; calc the product of i1 & i2
(define (inex* i1 i2)
  (local ((define m1 (inex-mantissa i1))
          (define m2 (inex-mantissa i2))
          (define s1 (inex-sign i1))
          (define s2 (inex-sign i2))
          (define e1 (inex-exponent i1)) 
          (define e2 (inex-exponent i2))
          (define product-m (* m1 m2))
          (define sum-e (+ (* s1 e1) (* s2 e2))))
    (cond [(< product-m 100) (create-inex product-m (/ sum-e (abs sum-e)) sum-e)]
          [(< product-m 1000) (create-inex (round (/ product-m 10)) (/ (add1 sum-e) (abs (add1 sum-e))) (add1 sum-e))]
          [else (create-inex (round (/ product-m 100)) (/ (+ sum-e 2) (abs (+ sum-e 2))) (+ sum-e 2))])))

(check-expect (inex* (create-inex 5 1 20)
                     (create-inex 5 1 30))
              (create-inex 25 1 50))
(check-expect (inex* (create-inex 25 1 20)
                     (create-inex 5 1 30))
              (create-inex 12 1 51))
(check-expect (inex* (create-inex 50 1 20)
                     (create-inex 50 1 30))
              (create-inex 25 1 52))
(check-expect (inex* (create-inex 50 1 20)
                     (create-inex 50 1 30))
              (create-inex 25 1 52))
(check-expect (inex* (create-inex 50 -1 4)
                     (create-inex 50 1 3))
              (create-inex 25 1 1))
(check-expect (inex* (create-inex 50 1 3)
                     (create-inex 50 -1 4))
              (create-inex 25 1 1))

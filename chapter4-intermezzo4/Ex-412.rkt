#lang htdp/isl+

#| Exercise 412.
Design inex+.
The function adds two Inex representations of numbers that have the same exponent.
The function must be able to deal with inputs that increase the exponent.
Furthermore, it must signal its own error if the result is out of range,
not rely on create-inex for error checking.
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
; calc the sum of i1 & i2
; constraint i1 & i2 have the same exponent
(define (inex+ i1 i2)
  (local ((define m1 (inex-mantissa i1))
          (define m2 (inex-mantissa i2))
          (define sum-ma (+ m1 m2))
          (define s1 (inex-sign i1))
          (define s2 (inex-sign i2))
          (define e1 (inex-exponent i1))
          (define e2 (inex-exponent i2)))
    (if (= e1 e2)
        (cond [(and (> sum-ma 99) (= e1 99) (= e2 99)) (error "the result is out of range")]
              [(< sum-ma 100) (make-inex sum-ma (inex-sign i1) e1)]
              [else (make-inex (round (/ sum-ma 100))
                               (inex-sign i1)
                               (add1 e1))])
        (cond [(= (* e1 s1) (sub1 (* e2 s2))) (inex+ i1 (create-inex (* 10 m2) s1 e1))] ; e1 < e2
              [(= (* e2 s2) (sub1 (* e1 s1))) (inex+ (create-inex (* 10 m1) s2 e2) i2 )])))) ; e1 > e2

(check-expect (inex+ (create-inex 4 1 2)
                     (create-inex 5 1 2))
              (create-inex 9 1 2))`
(check-expect (inex+ (create-inex 50 1 2)
                     (create-inex 60 1 2))
              (create-inex 1 1 3))
(check-expect (inex+ (create-inex 75 1 2)
                     (create-inex 80 1 2))
              (create-inex 2 1 3))
(check-error (inex+ (create-inex 75 1 99)
                    (create-inex 80 1 99))
             "the result is out of range")
(check-expect (inex+ (create-inex 1 1 0)
                     (create-inex 1 -1 1))
              (create-inex 11 -1 1))
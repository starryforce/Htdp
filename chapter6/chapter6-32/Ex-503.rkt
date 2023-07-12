#lang htdp/isl+

(define NO_RESULT "no row starts differs from 0")
; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate '((0 4 5) (0 2 3))) NO_RESULT)
(define (rotate M0)
  (local (; Matrix Number -> Matrix
          ; finds a row that doesn't start with 0 and
          ; uses it as the first one
          ; generative moves the first row to last place 
          ; accumulator a is the number of rows not checked.
          (define (rotate/a M a)
            (cond
              [(zero? a) (error NO_RESULT)]
              [(not (= (first (first M)) 0)) M]
              [else
               (rotate/a (append (rest M) (list (first M))) (sub1 a))])))
    (rotate/a M0 (length M0))))

; 5000 x 5000

(define COUNT 10000)
(define ex (build-list COUNT (lambda (i) (list (if (= i (- COUNT 1)) 1 0) 0 0 0 0))))

(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate '((0 4 5) (0 2 3))) NO_RESULT)
(define (rotate.v2 M0)
  (local (; Matrix Matrix -> Matrix 
          ; accumulator seen is Matrix which contains
          ; all rows that M lacks from M0 in reverse order
          (define (rotate/a M seen)
            (cond
              [(empty? M) (error NO_RESULT)]
              [(not (= (first (first M)) 0)) (append (list (first M)) (reverse seen) (rest M))]
              [else (rotate/a (rest M) (cons (first M) seen))])))
    (rotate/a M0 '())))

(time (rotate ex))    ; cpu time: 328 real time: 590 gc time: 171
(time (rotate.v2 ex)) ; cpu time: 0 real time: 3 gc time: 0
#lang htdp/isl+

(require 2htdp/abstraction)

(define ex (build-list 10 (lambda (i) i)))

; [X -> Boolean] [List-of X] -> [X | #f]
(define (and-map d l)
  (local (; X  [X | #f] -> [X | #f]
          (define (f cur prev)
            ; if #f occurs, always be #f
            (if (false? prev)
                #false
                (if (false? (d cur))
                    #false
                    cur )))
          )
    (foldl f #true l)))


(check-expect (for/and ([i 10]) (> (- 9 i) 0))
              (and-map (lambda (i) (> (- 9 i) 0)) ex))

(check-expect (for/and ([i 10]) (if (>= i 0) i #false))
              (and-map (lambda (i) (if (>= i 0) i #false)) ex))


(define (or-map d l)
  (local (; X [X |
          ; prev is #f means every item before this is #f
          (define (f cur prev)
            (if (false? prev)
                (if (false? (d cur)) #false cur)
                prev)))
    (foldl f #false l)))

(check-expect (for/or ([i 10]) (if (= (- 9 i) 0) i #false))
              (or-map (lambda (i) (if (= (- 9 i) 0) i #false)) ex))
(check-expect (for/or ([i 10]) (if (< i 0) i #false))
              (or-map (lambda (i) (if (< i 0) i #false)) ex))


; [X -> Number] [List-of X] -> Number
(define (fold-sum calc l)
  (local (; X Number -> Number
          ; calculate sum of result of (calc cur) & prev
          (define (f cur prev)
            (+ (calc cur) prev)))
    (foldl f 0 l)))

(check-expect (for/sum ([c "abc"]) (string->int c))
              (fold-sum string->int (explode "abc")))


; [X] [X -> Number] [List-of X] -> Number
(define (fold-product calc l)
  (local (; X Number -> Number
          ; calculate product of result of (calc cur) & prev
          (define (f cur prev)
            (* (calc cur) prev)))
    (foldl f 1 l)))

(check-expect (for/product ([c "abc"]) (+ (string->int c) 1))
              (fold-product (lambda (c) (+ (string->int c) 1)) (explode "abc")))

; [X] [X -> 1String] [List-of X] -> String
(define (fold-string trans l)
  (implode (map trans l)))

(define a (string->int "a"))
(check-expect (for/string ([j 10]) (int->string (+ a j))) (fold-string (lambda (j) (int->string (+ a j))) ex))

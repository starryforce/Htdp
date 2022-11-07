#lang htdp/isl+

; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))

; Number ->
; [[List-of Posn] -> Boolean]
; determine if Posn in l in within a WIDTH * HEIGHT rectangle
; and have count items
(define (n-inside-playground? count)
  (lambda (l)
    (local (; x-coordinate list
            (define l-x (map posn-x l))
            ; y-coordinate list
            (define l-y (map posn-y l))
            (define min-x (apply min l-x))
            (define max-x (apply max l-x))
            (define min-y (apply min l-y))
            (define max-y (apply max l-y))
            )
      (and (= (length l) count)
           (<= (- max-x min-x) WIDTH)
           (<= (- max-y min-y) HEIGHT)))))


(check-expect [(n-inside-playground? 3) (list (make-posn 1 20) (make-posn 100 20) (make-posn 20 30))] #true)
(check-expect [(n-inside-playground? 3) (list (make-posn 1 20) (make-posn 100 20))] #false)
(check-expect [(n-inside-playground? 3) (list (make-posn 1 20) (make-posn 1000 20) (make-posn 20 30))] #false)


(define (random-posns/bad n)
  (build-list
    n
    (lambda (i)
      (make-posn 1 1))))

(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))

#lang htdp/isl+

; [List-of X] -> Number
(check-expect (how-many '(1 2 3 4 5)) 5)
(define (how-many alox0)
  (local (; [List-of X] ??? -> Number
          ; determine the number of items on alox
          ; accumulator a is the number of items alox
          ; lacks from alox0
          (define (how-many/a alox a)
            (cond [(empty? alox) a]
                  [else (how-many/a (rest alox) (add1 a))])))
    (how-many/a alox0 0)))

; same O(n)

#|
(how-many '(1 2 3))
(how-many/a '(1 2 3) 0)
(how-many/a '(2 3) (add1 0))
(how-many/a '(2 3) 1)
(how-many/a '(3) (add1 1))
(how-many/a '(3) 2)
(how-many/a '() (add1 2))
(how-many/a '() 3)
3
at most 1 pending add1 at the same time
|#

#lang htdp/isl+

; [X Y] [X -> Y] [List-of X]
; -> [List-of Y]
; a version of map build from fold
(define (map-from-fold f aloi)
  (local (; X [List-of Y] -> [List-of Y]
          ; append transformed x to y
          (define (fn i l)
            (cons (f i) l))
          )
    (foldr fn '() aloi)))

(check-expect (map-from-fold (lambda (x) (* x 2)) (list 1 2 3 4))
              (list 2 4 6 8))
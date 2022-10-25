#lang htdp/isl

; A Row is [List-of Number]
; A Matrix is [List-of Row]
; the items quantity in Row is equal to the item number in Matrix


; Number -> [List-of Row]
; generate a identity matrices of n rows
(define (identityM n)
  (local (; Number '() -> [List-of Number]
          (define (make-matrix row-no l)
            (cond [(= row-no n) l]
                  [else (cons (local (; Number '() -> [List-of Number]
                                      ; generate a l length list fill with 0 except at position row-no is 1
                                      (define (make-row cur l)
                                        (cond [(= cur n) l]
                                              [else (cons (if (= cur row-no) 1 0) (make-row (add1 cur) l))])))
                                ; - IN -
                                (make-row 0 '()))
                              (make-matrix (add1 row-no) l))])))
    ; - IN -
    (make-matrix 0 '())))

(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
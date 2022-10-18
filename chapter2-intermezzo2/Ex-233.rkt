#lang htdp/bsl+

(define q1 `(0 ,@'(1 2 3) 4))
(define a1 (list 0 1 2 3 4))
(check-expect q1 a1)

(define q2 `(("alan" ,(* 2 500))
             ("barb" 2000)
             (,@'("carl" " , the great")   1500)
             ("dawn" 2300)))

(define a2 (list (list "alan" 1000)
                 (list "barb" 2000)
                 (list "carl" " , the great" 1500)
                 (list "dawn" 2300)))
(check-expect q2 a2)

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
 
; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,(number->string n)))

(define q3 `(html
             (body
              (table ((border "1"))
                     (tr ((width "200"))
                         ,@(make-row '( 1  2)))
                     (tr ((width "200"))
                         ,@(make-row '(99 65)))))))
(define a3 (list `html
             (list `body
                   (list `table
                         (list (list `border "1"))
                         (cons `tr (cons (list (list `width "200")) (make-row (list 1 2))))
                         (cons `tr (cons (list (list `width "200")) (make-row (list 99 65))))))))
(check-expect q3 a3)
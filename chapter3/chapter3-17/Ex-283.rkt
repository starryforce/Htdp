#lang htdp/isl+

(define th 20)

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

;((lambda (ir) (<= (IR-price ir) th))
;   (make-IR "bear" 10))


;(map (lambda (x) (* 10 x))
;     '(1 2 3))


;(foldl (lambda (name rst)
;         (string-append name ", " rst))
;       "etc."
;       '("Matthew" "Robby"))

(filter (lambda (ir) (<= (IR-price ir) th))
        (list (make-IR "bear" 10)
              (make-IR "doll" 33)))
#lang htdp/isl
	
(define-struct IR
  [name price])
; An IR is a structure:
;   (make-IR String Number)
; An Inventory is one of: 
; – '()
; – (cons IR Inventory)

(define ex (list
                  (make-IR "doll" 21.0)
                  (make-IR "bear" 13.0)
                  (make-IR "a" 13.0)
                  (make-IR "b" 6.0)
                  (make-IR "c" 13.0)
                  (make-IR "d" 0.67)
                  (make-IR "e" 13.0)
                  (make-IR "f" 2.0)
                  (make-IR "g" 13.0)
                  (make-IR "h" 4)
                  (make-IR "i" 0.13)))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))


(define (extract1.v2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local (; stash each step's result tempraroly
             (define tmp (extract1.v2 (rest an-inv))))
       ; - IN -
       (cond
         [(<= (IR-price (first an-inv)) 1.0)
          (cons (first an-inv) tmp)]
         [else tmp]))]))

; Not at all,
; because no matter which expression is calced, the nature recusion will
; just evaluate only once
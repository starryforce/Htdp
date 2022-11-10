#lang htdp/isl+

(define-struct inventory [name desc cost price])
; An Inventory is a structure:
; (make-inventory String String Number Nubmer)
; interpretation (make-inventory n d c p) represents
; an item whose name is n, has a description of d,
; acquisition price is c, and the recommended sales price is p

(define inventory1 (make-inventory "banana" "it's a fruit" 4 7))
(define inventory2 (make-inventory "carrot" "it's a vegetable" 2.4 4))
(define inventory3 (make-inventory "sofa" "its' s a funiture" 500 600))
(define ex (list inventory1 inventory2 inventory3))

; [List-of Inventory] -> [List-of Inventory]
; sorts a list of inventory records by the difference between the two prices
(define (inventory-tidy aloi)
  (sort aloi (lambda (i1 i2)
               (< (inventory-price i1)
                  (inventory-price i2))
               )))

(check-expect (inventory-tidy ex) (list inventory2 inventory1 inventory3))

#lang htdp/isl

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

; Number [List-of Inventory] -> [List-of Inventory]
; produces a list of all those structures whose sales price is below ua.
(define (eliminate-expensive ua aloi)
  (local (; Inventory -> Boolean
          ; determine if inventory's price is below ua.
          (define (cheap? i)
            (< (inventory-price i) ua))
          )
    (filter cheap? aloi)))

(check-expect (eliminate-expensive 3 ex) '())
(check-expect (eliminate-expensive 5 ex) (list inventory2))
(check-expect (eliminate-expensive 100 ex) (list inventory1 inventory2))

; String [List-of Inventory] -> [List-of Inventory]
; produces a list of inventory records that do not use the name ty from aloi
(define (recall ty aloi)
  (local (; Inventory -> Boolean
          ; determine if i's name is different from ty)
          (define (diff? i)
            (not (string=? (inventory-name i) ty))))
    (filter diff? aloi)))

(check-expect (recall "sofa" ex) (list inventory1 inventory2))


; [List-of String] [List-of String] -> [List-of String]
; selects all those from ly that are also on lx.
(define (selection lx ly)
  (local (; String -> Boolean
          ; determine if s exsit on lx
          (define (exsit? s)
            ; ? -> Boolean
            (local (; [List-of String] -> Boolean
                    ; determine if lx contains x
                    (define (contains? item)
                      (string=? item s)))
              (ormap contains? lx))))
    (filter exsit? ly)))

(define list1 (list "a" "b" "c" "d" "e"))
(define list2 (list "d" "e" "f"))

(check-expect (selection list1 list2) (list "d" "e"))
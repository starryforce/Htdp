#lang htdp/isl

(define ex1 (list "a" "b" "c"))

; [List-of 1String] -> [List-of [List-of 1String]]
; produces the list of all prefixes
; A list p is a prefix of l if p and l are the same up through all items in p
(define (prefixes alos)
  (local (; 1String [List-of [List-of 1String]] -> [List-of [List-of 1String]]
          ;
          (define (fn extra l)
            (cons (cons extra  (if (empty? l)
                                   '()
                                   (first l))) l))
          )
    (foldr fn '()  alos)))

(check-expect (prefixes ex1) (list (list "a" "b" "c") (list "b" "c") (list "c")))



         
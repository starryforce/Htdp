#lang htdp/isl

; [List-of Number] [Number Number -> Boolean]
; -> [List-of Number]
(define (sort-n alon sorter-n)
  alon)

; [List-of String] [String String -> Boolean]
; -> [List-of String]
(define (sort-s alos sorter-s)
  alos)

; [X] [List-of X] [X X -> Boolean]
; -> [List-of X]
(define (sort-self aloi sorter) aloi)

; [List-of IR] [IR IR -> Boolean]
; -> [List-of IR]